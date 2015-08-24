{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

--------------------------------------------------------------------------------
-- |
-- Module         : Main
-- Copyright      : (C) 2014-2015 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This application is used to automatically generate web page bodies listing
-- the available courses at the department of physics at the University of
-- Helsinki. The data is parsed from a confluence Wiki Source Table containing
-- the information needed to either directly generate the list or lookup more
-- information from different web sites.
--
-- The application takes into consideration internationalization ('I18N') for
-- at least Finnish, Swedish and English, more languages might be supported in
-- the future.
--
-- The user guide for operating the software can be found at
-- <https://github.com/SimSaladin/opetussivut>
--
-- See /config.yaml/ for configuration options.
--
--------------------------------------------------------------------------------

module Main where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Reader
import           Codec.Text.IConv           (convert)
import           Data.Char
import           Data.Function              (on)
import qualified Data.List                                      as L
import           Data.Map                   (Map)
import qualified Data.Map                                       as Map
import           Data.Monoid                ((<>))
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                                      as T
-- The 'Data.Text.Lazy' imports are needed when streaming large quantities of
-- 'Text' data and you want to be efficient doing so.
import qualified Data.Text.Lazy                                 as LT
import qualified Data.Text.Lazy.IO                              as LT
import qualified Data.Text.Lazy.Encoding                        as LT
import           Data.Time
-- Used to load the settings file.
import qualified Data.Yaml                                      as Yaml
import           Network.HTTP.Conduit       (simpleHttp)
import           Text.Blaze.Html            (preEscapedToHtml)
import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Text.Hamlet
import           Text.Julius
import           Text.Markdown
import           Text.Regex
import qualified Text.XML                                       as XML
import           Text.XML.Cursor
import           System.Directory
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.IO.Unsafe           (unsafePerformIO)
import           GHC.Generics
{- Used to print to the console, during some of the parsing... Other prints are
   done through the IO monad with the @putStrLn@ function.
-}
import           Debug.Trace


{- | The entry point of the application.

    It reads the /config.yaml/ file into memory before doing anything else.
    After that it access the page data of the selected page, either from cache
    or from the Wiki Table based on the command line arguments used.

    When the page data is fetched it starts the 'Table' generation by calling
    the @'parseTable'@ function and later loops over the different 'Lang'uages
    found in the /config.yaml/ page configuration section and renders a
    different page body for each of the languages.
-}
main :: IO ()
main = do
    putStrLn ("============================================================\n" ++
              " Running Application\n" ++
              "============================================================\n" ++
              " * Decoding config.yaml")
    Yaml.decodeFileEither "config.yaml" >>= either (error . show) (runReaderT go)
  where
    go = do
        Config{..} <- ask
        forM_ pages $ \pc@PageConf{..} -> do
            liftIO $ putStrLn (" * Reading data from Wiki page ID: " ++ pageId)
            pageData <- getData pageId
            case pageData of
                Nothing -> liftIO $ putStrLn "!!! Warning: failed to fetch doc, not updating the listings..."
                Just pageData' -> do
                    table <- parseTable pageData'
                    forM_ languages $ \lang -> do
                        liftIO $ putStrLn (" *     Language: " ++ show lang)
                        renderTable rootDir lang pc table


-- =============================================================================
-- * Types
-- =============================================================================


{- | Creates a monad that handles the assignment of the @Config@ type. It uses
	the functionality of both the @Reader@ monad and the @IO@ monad.

	@ReaderT@ monad transformer
	@Config@ the return value of @ask@ function
	@IO@ monad on which the @Reader@ acts.

	By using this monad, one can read all values of type @Config@ and perform
	IO actions.
-}
type M = ReaderT Config IO


{- | The 'Lang' type is used as key for looking up the translations from the
    internationalization ('I18N') database found in the /config.yaml/ file.

    It is used with acronyms of the languages: @fi@, @se@, @en@, ...
-}
type Lang = Text


{- | Properties for generating individual web page bodies.

    The 'PageConf' data type is used as a data holder for page information
    of the generated web pages. Where they are stored locally, what the page
    title is etc.
-}
data PageConf = PageConf {
              -- Department web page properties
                pageId          :: String         -- ^ Wiki Table ID
              , pageUrl         :: Map Lang Text  -- ^ Local path
              , pageTitle       :: Map Lang Text  -- ^ Local title
              } deriving Generic
instance Yaml.FromJSON PageConf


{- | Overall properties used by the module to generate HTML code from the
    source 'Table' found in the wiki pages.

    The 'Config' data type is used to read the configuration properties from
    the /config.yaml/ file. It holds references to all of the different
    property fields and are accessed by their name in the /config.yaml/ file.
    The fields are filled with the type constructor of @Config@ by a call to 
	the @'ask'@ function through the @M@ monad.
-}
data Config   = Config {
              -- File handling properties
                rootDir         :: FilePath     -- ^ Local file path (root).
              , cacheDir        :: FilePath     -- ^ Local cache path.
              , fetchUrl        :: String       -- ^ URL to all Wiki Tables.
              , weboodiUrl      :: Text         -- ^ URL to all WebOodi pages.
              , oodiNameFile    :: FilePath     -- ^ File for course name translations.
              -- Page generation properties
              , languages       :: [Lang]       -- ^ All supported 'Lang'uages (@fi@, @en@, @se@, etc).
              , pages           :: [PageConf]   -- ^ References to the different subject pages.
              -- Internationalization properties
              , i18n            :: I18N         -- ^ Database with translations.
              -- Wiki Table properties
              , categoryLevel   :: Int
              , categories      :: [[Text]]     -- ^ List of lists of 'Category's (used to nestle the categories for the 'Course's).
              , columnHeaders   :: ColumnHeader -- ^ The @columnHeaders@ section of the /config.yaml/ file.
              , classCur        :: Text         -- ^ HTML class for showing if the course is available this year.
              } deriving Generic
instance Yaml.FromJSON Config


{- | Source 'Table'. Consists of a time stamp, a list of 'Header's and a list
    of 'Course's (the 'Course' objects are used as rows in the 'Table').
-}
data Table        = Table UTCTime [Header] [Course]
                    deriving (Show, Read)


{- | Synonym of 'Text'. Used as column 'Header's when reading the source
    'Table'.
-}
type Header       = Text


{- | A row in source 'Table'. Each cell of the 'Table' row is mapped to a
    'Header', and each cell can have multiple 'Category's coupled to it (see
    'Category'). The 'ContentBlock' contains all the information read from the
    Wiki Table for the specified 'Course'.
-}
type Course       = ([Category], Map Header ContentBlock)


{- | Synonym of 'Text'. First column in source 'Table'. Each 'Course' can
    have several 'Category's (eg. @Pääaineopinnot@, @Perusopinnot@,
    @Pakolliset@, etc).
-}
type Category     = Text


-- | @\<td\>@ HTML tag in source 'Table'. Used to fill the 'Course' row.
type ContentBlock = Text


{- | Internationalization database.

    Map of a Finnish 'Text' phrase (fragment) to a list of 'Text's mapped to
    'Lang'uages. This way it is easy to translate a specific Finnish phrase to
    one of the supported 'Lang'uages.

    By first looking up the Finnish phrase from the internationalization
    ('I18N') database, one gets a 'Map' of languages to translations (proper
    error handling is needed to fallback when looking up a translation that
    does not exist see @'toLang'@ for implementation). Then one will only have
    to lookup up the desired language from the result.

    > Map.lookup [Language] $ Map.lookup [Finnish phrase] [I18N database]
-}
type I18N         = Map Text (Map Lang Text)


{- | ColumnHeader database.

    The different properties found in the @columnHeaders@ section of the
    /config.yaml/ file. All the functions for this property is very similar to
    the functions of the 'I18N' type.

    When adding more column headers remember to check out the helper functions
    inside the function body of @'tableBody'@.
-}
type ColumnHeader = Map Text (Map Text Text)

-- =============================================================================
-- * Utility functions
-- =============================================================================


{- | Appends /.html/ to the end of the argument. It's mainly used as a helper
    function when creating the body files locally, this way it's possible to
    generate links between the different web pages.
-}
toUrlPath :: Text   -- ^ The URL without /.html/.
          -> Text   -- ^ The URL with /.html/ at the end.
toUrlPath  = (<> ".html")


{- | Prepends the argument with the value of the @root@ parameter, and appends
    /.body/ to the end of the argument.

    > root [arg] .body

    The PHP-engine used on physics.helsinki.fi utilizes a own /.html/ file
    extension (/.body/) to show HTML output in the center of the page.
-}
toFilePath :: FilePath  -- ^ The root directory.
           -> Text      -- ^ The file name.
           -> FilePath  -- ^ @root/filename.body@.
toFilePath root = (root <>) . T.unpack . (<> ".body")


{-| A hack, because confluence HTML is far from the (strictly) spec.

    WebOodi's HTML is just horrible (imo it's not even HTML), so we use another
    regex to parse it (before the XML parser is used).
-}
regexes :: [String -> String]   -- ^ A list of functions for removing HTML-tags
regexes = [ rm "<meta [^>]*>", rm "<link [^>]*>", rm "<link [^>]*\">", rm "<img [^>]*>"
          , rm "<br[^>]*>", rm "<col [^>]*>" ]
  where
    rm s i = subRegex (mkRegexWithOpts s False True) i ""


{- | Fetch a translation from the 'I18N' database, found in /config.yaml/ under
    the 'I18N' section.

    If the 'Text' that is to be translated can't be found in the given database
    it'll fall back on the given 'Text' (the Finnish version). If the selected
    'Lang'uage in this case is not Finnish it will also warn the user that no
    translation for the selected 'Text' can be found.
-}
toLang :: I18N  -- ^ The 'I18N' database to fetch translations from.
       -> Lang  -- ^ The 'Lang'uage to fetch translation for.
       -> Text  -- ^ The 'Text' to translate.
       -> Text  -- ^ The translation of the input 'Text'.
toLang db lang key = maybe (trace ("!!! Warning: no i18n db for key `" ++ T.unpack key ++ "'") key)
                           (fromMaybe fallback . Map.lookup lang) (Map.lookup key db)
  where
    fallback | "fi" <- lang = key
             | otherwise    = trace ("!!! Warning: no i18n for key `" ++ T.unpack key ++ "' with lang `" ++ T.unpack lang ++ "'") key


{- | Access the 'ColumnHeader' information with name of the column and with a
    key to the information desired.

    Returns the value of the desired column information. If the column name
    doesn't exist it'll show a warning in the output and an empty 'Text' is
    returned instead.
-}
columnInfo :: ColumnHeader  -- ^ The 'ColumnHeader' to look for information in.
           -> Text          -- ^ The column information to look for (see /config.yaml/ for alternatives).
           -> Text          -- ^ The column name to look for.
           -> Text          -- ^ The value of the column information.
columnInfo db key column = maybe (trace ("!!! Warning: no column in db with name `" ++ T.unpack column ++ "'") column)
                                 (fromMaybe "" . Map.lookup key) (Map.lookup column db)


{- | Wrapper function to access the @title@ information of the @column@. Calls
    the @'columnInfo'@ function with \"title\" as the desired information.
-}
columnTitle :: ColumnHeader -- ^ The 'ColumnHeader' to look for information in.
            -> Text         -- ^ The column name to look for.
            -> Text         -- ^ The value of the column information with key \"title\".
columnTitle db column = columnInfo db "title" column


{- | Wrapper function to access the @width@ information of the @column@. Calls
    the @'columnInfo'@ functio with \"width\" as the desired information.
-}
columnWidth :: ColumnHeader -- ^ The 'ColumnHeader' to look for information in.
            -> Text         -- ^ The column name to look for.
            -> Text         -- ^ The value of the column information with key \"width\".
columnWidth db column = columnInfo db "width" column


{- | Lookup a translation from any given map, containing 'y' types mapped to
    'Lang' types. It only returns 'Just' values of the result.
-}
lookupLang :: Lang      -- ^ The 'Lang'uage to look for.
        -> Map Lang y   -- ^ A map to fetch type 'y' from.
        -> y            -- ^ The value of type 'y' found in the map.
lookupLang i = fromJust . Map.lookup i


{- | This function removes all extra whitespaces between words in the input
    'Text' and removes some unwanted text from the input 'Text'.

    First it creates a list of 'Text's by splitting the input 'Text' on all
    newline characters, then it removes all extra whitespaces between the words
    in all rows in the created list, after this it merges all the rows into one
    'Text' object separated by space characters.

    It then replaces some unwanted text on the combined 'Text' object and
    removes some unwanted text completely (cells only containing ' ', ',', '-'
    or '!' characters).
-}
normalize :: Text   -- ^ The 'Text' to normalize.
          -> Text   -- ^ The normalized 'Text'.
normalize =
    T.dropAround (`elem` " ,-!")
    . T.replace "ILMOITTAUTUMINEN PUUTTUU" ""
    . T.unwords . map (T.unwords . T.words) . T.lines


-- =============================================================================
-- * Weboodi stuff
-- =============================================================================


{- | Fetch the 'Course' name from the WebOodi URL. It looks for a HTML tag
    containing the text @tauluotsikko\"?>@ to find the name of the 'Course'. It
    then replaces some sub strings in the 'Course' name with Unicode characters.
-}
getOodiName :: Text         -- ^ 'Text' containing a raw version of the 'Course' name.
            -> Maybe Text   -- ^ The Unicode formatted version of the 'Course' name.
getOodiName =
    fmap (T.pack
          . sub "&aring;" "å"
          . sub "&Aring;" "Å"
          . sub "&auml;" "ä"
          . sub "&Auml;" "Ä"
          . sub "&ouml;" "ö"
          . sub "&Ouml;" "Ö"
          . sub "&#x3a;" ":"
          . sub "&#x27;" "'"
          . sub "&#x28;" "("
          . sub "&#x29;" ")"
          . sub "&#x3b;" ";"
          . head) . matchRegex (mkRegexWithOpts s True False) . T.unpack
  where
    s         = "tauluotsikko\"?>[0-9 ]*(.*),[^,]*<"
    sub a b i = subRegex (mkRegexWithOpts a False True) i b


-- TODO: Move this into the 'webOodiLink' function
{- | Helper method to select the correct 'Lang'uage when using WebOodi. The
    'Lang'uage is changed by switching a number in the URL:

        * 1 : fi (Finnish version)

        * 2 : se (Swedish version)

        * 6 : en (English version)

    If another 'Lang'uage than the provided ones is used, the function returns
    an empty 'String' instead of a 'Num'ber.
-}
weboodiLang :: Lang     -- ^ The 'Lang' to use on WebOodi.
            -> Text     -- ^ A 'Text' string consisting of the number to switch to in the WebOodi URL.
weboodiLang lang
            | "fi" <- lang = "1"
            | "se" <- lang = "2"
            | "en" <- lang = "6"
            | otherwise    = ""


{- | Creates a hyperlink to use for accessing the selected 'Lang'uage version
    of WebOodi. This webpage is later used to access translations for the
    different course names, when altering the language on the course listing.

    The return value has the form of:
    <https://weboodi.helsinki.fi/hy/opintjakstied.jsp?html=1&Kieli=&Tunniste=[page ID]>.
-}
weboodiLink :: Text     -- ^ The base URL of WebOodi.
            -> Lang     -- ^ Language to use on WebOodi.
            -> Text     -- ^ WebOodi page ID.
            -> Text     -- ^ The concatenated URL.
weboodiLink url lang pageId = url <> weboodiLang lang <> "&Tunniste=" <> pageId


{- | This is a helper function that gives a 'MVar' variable with a @('Lang',
    [WebOodi code])@ mapped to a course name (for that specific 'Lang'uage).

    This function is used when accessing the /oodi.names/ file for course name
    translations. The /oodi.names/ file is generated from accessing the WebOodi
    web page during the fetching process.

    Initially the 'MVar' is empty. The 'MVar' variable in this case is used to
    only have to fill the value once (when reading one of the languages), to
    save computation time.
-}
oodiVar :: MVar (Map (Lang, Text) Text) -- ^ A 'MVar' containing a 'Map' between a @('Lang', [WebOodi code])@ and a @[Course name]@.
oodiVar = unsafePerformIO newEmptyMVar
{-# NOINLINE oodiVar #-}
{-  If one is using the @unsafePerformIO@ function like this, it is
    recommended to use the @NOINLINE@ pragma on the function to
    prevent the compiler from adding it everywhere in the compiled code.
-}


{- | Read the course names from the @oodiNameFile@.

    Helper function for reading the @oodiNameFile@ to get translations for the
    course names. If the file can't be found it'll return an empty map.

    If the file exists this function returns a 'Map' of translations for the
    different 'Course' names, otherwise it'll return an empty 'Map'.
-}
readOodiNames :: M (Map (Lang, Text) Text)  -- ^ A 'Map' of all the WebOodi translations.
readOodiNames = do
    Config{..} <- ask
    exists <- liftIO $ doesFileExist oodiNameFile
    if exists
        then liftIO $ read <$> readFile oodiNameFile
        else return Map.empty


-- TODO: Check what this method does, step by step.
{- | Get a translated name for a specific 'Course', by looking it up from
    WebOodi.
-}
i18nCourseNameFromOodi :: Lang              -- ^ The 'Lang'uage to lookup.
                       -> Text              -- ^ The WebOodi page ID.
                       -> M (Maybe Text)    -- ^ The translation if found.
i18nCourseNameFromOodi lang pageId = do
    Config{..} <- ask

    ov         <- liftIO $ tryTakeMVar oodiVar
    oodiNames  <- case ov of
        Just x  -> return x
        Nothing -> readOodiNames
    liftIO $ putMVar oodiVar oodiNames

    case Map.lookup (lang, pageId) oodiNames of
        Just name -> return $ Just name
        Nothing   -> do
            raw <- liftIO $ fetch8859 (T.unpack $ weboodiLink weboodiUrl lang pageId)
            case getOodiName raw of
                Nothing   -> return Nothing
                Just name -> do
                    let newNames = Map.insert (lang, pageId) name oodiNames
                    liftIO $ do _ <- swapMVar oodiVar newNames
                                writeFile oodiNameFile (show newNames)
                    return $ Just name


{- | Reads the web page at the given URL and returns a utf-8 converted version
    of the page. WebOodi is encoded in iso-8859-1 encoding.
-}
fetch8859 :: String     -- ^ URL with iso-8859-1 encoding.
          -> IO Text    -- ^ The web page converted to utf-8.
fetch8859 url = LT.toStrict . LT.decodeUtf8 . convert "iso-8859-1" "utf-8" <$> simpleHttp url


-- =============================================================================
-- * Rendering
-- =============================================================================


-- | Create the final HTML version of the web page from the cached one.
renderTable :: FilePath     -- ^ The root to where the HTML file should be saved.
            -> Lang         -- ^ The currently used language.
            -> PageConf     -- ^ More specific information of the current web page being created.
            -> Table        -- ^ The source table to use when creating the web page
            -> M ()         -- ^ ReaderT Config IO, from the generated HTML file.
renderTable root lang pc@PageConf{..} table =
    ask >>= lift . LT.writeFile fp . renderMarkup . tableBody lang pc table
  where
    fp = toFilePath root $ lookupLang lang pageUrl


-- =============================================================================
-- * Content
-- =============================================================================


{- | How to render the data of the selected 'Table' into HTML using WebOodi to
    lookup course names in different languages.
-}
tableBody :: Lang           -- ^ The currently used 'Lang'uage.
          -> PageConf       -- ^ More specific information of the current web page being created.
          -> Table          -- ^ The source 'Table' to use when creating the web page.
          -> Config         -- ^ 'Config'uration containing specific information about the source 'Table' and translation data.
          -> Html           -- ^ The generated HTML code.
tableBody lang page (Table time _ tableContent) cnf@Config{..} =
    let
        -- | Helper function to get the 'I18N' translation of the input.
        i18nTranslationOf :: Text   -- ^ The 'Text' to translate.
                          -> Text   -- ^ Translation of the input.
        i18nTranslationOf            = toLang i18n lang

        {- | Overriding the translations for some 'String's, for usage with the
            /periodi/ column of the 'Table'.
        -}
        i18nTranslationOfPeriod :: Text     -- ^ The cell content to be translated.
                                -> Text     -- ^ The translation.
        i18nTranslationOfPeriod cell
                                | "?"      <- cell = cell
                                | "I"      <- cell = cell
                                | "I-II"   <- cell = cell
                                | "I-III"  <- cell = cell
                                | "I-IV"   <- cell = cell
                                | "I, III" <- cell = cell
                                | "II"     <- cell = cell
                                | "II-III" <- cell = cell
                                | "II-IV"  <- cell = cell
                                | "II, IV" <- cell = cell
                                | "III"    <- cell = cell
                                | "III-IV" <- cell = cell
                                | "IV"     <- cell = cell
                                | ""       <- cell = cell
                                -- If non of the above \"exceptions\" is found in the listing,
                                -- the cell content will be translated.
                                | otherwise        = i18nTranslationOf cell

        -- | Helper method to fetch a translation of the 'Course' name
        translateCourseName :: Text         -- ^ WebOodi course code.
                            -> Maybe Text   -- ^ Translation of the 'Course' name.
        translateCourseName     code = unsafePerformIO $
            runReaderT (i18nCourseNameFromOodi lang code) cnf


        -- Helper functions to access the different column header properties
        colTitle          col = columnTitle columnHeaders col
        colCodeTitle          = colTitle "colCode"
        colCourseNameTitle    = colTitle "colCourseName"
        colPeriodTitle        = colTitle "colPeriod"
        colRepeatsTitle       = colTitle "colRepeats"
        colLangTitle          = colTitle "colLang"
        colWebsiteTitle       = colTitle "colWebsite"

        colWidth          col = columnWidth columnHeaders col
        colCodeWidth          = colWidth "colCode"
        colCourseNameWidth    = colWidth "colCourseName"
        colPeriodWidth        = colWidth "colPeriod"
        colRepeatsWidth       = colWidth "colRepeats"
        colLangWidth          = colWidth "colLang"
        colWebsiteWidth       = colWidth "colWebsite"


        ------------------------------------------------------------------------
        -- course table
        ------------------------------------------------------------------------

        {- | Function for generating the nestled @\<div.courses\>@ HTML-tags.

            If the maximum number of @categories@ that can be nestled not is
            reached it'll continue trying to add 'Category' titles until the
            maximum is reached. After that the 'Table' row is created.
        -}
        courseTable n rows
            | n < length categories = withCat n rows $ courseTable (n + 1)
            | otherwise             =
                [shamlet|
                    <table>
                        $forall c <- rows
                            <tr data-taso="#{fromMaybe "" $ catAt cnf categoryLevel c}"
                                data-kieli="#{getCellContent colLangTitle c}"
                                data-lukukausi="#{getCellContent "lukukausi" c}"
                                data-pidetaan="#{getCellContent "pidetään" c}">

                                <td style="width:#{colCodeWidth}">
                                    <a href="#{weboodiLink weboodiUrl lang $ getCellContent colCodeTitle c}">
                                        <b>#{getCellContent colCodeTitle c}

                                <td style="width:#{colCourseNameWidth}">

                                    $maybe name <- translateCourseName (getCellContent colCodeTitle c)
                                        #{name}
                                    $nothing
                                        #{getCellContent colCourseNameTitle c}

                                    $with op <- getCellContent "op" c
                                        $if not (T.null op)
                                            \ (#{op} #{i18nTranslationOf "op"})

                                <td.compact style="width:#{colPeriodWidth}"  title="#{getCellContent colPeriodTitle c}">#{i18nTranslationOfPeriod $ getCellContent colPeriodTitle c}
                                <td.compact style="width:#{colRepeatsWidth}" title="#{getCellContent colRepeatsTitle c}">#{getCellContent colRepeatsTitle c}
                                <td.compact style="width:#{colLangWidth};font-family:monospace" title="#{getCellContent colLangTitle c}">
                                    $case T.words (getCellContent colLangTitle c)
                                        $of []
                                        $of rows
                                            <b>#{head rows}
                                            $if null $ tail rows
                                            $else
                                                (#{T.intercalate ", " $ tail rows})

                                <td.compact style="width:#{colWebsiteWidth}" title="#{colWebsiteTitle}">
                                    $maybe p <- getCellContentMaybe colWebsiteTitle c
                                        $if not (T.null p)
                                            \ #
                                            <a href="#{p}">#{i18nTranslationOf colWebsiteTitle}
                |]


        ------------------------------------------------------------------------
        -- if it begins with a number, apply appropriate header
        ------------------------------------------------------------------------

        {- | Helper function to select the correct HTML-header tag for the
            current 'Category'.
        -}
        catHeader n category =
            [shamlet|
                $case n
                    $of 0
                        <h1>
                            #{translation}
                    $of 1
                        <h2>
                            #{translation}
                    $of 2
                        <h3>
                            <i>#{translation}
                    $of 3
                        <h4>
                            #{translation}
                    $of 4
                        <h5>
                            #{translation}
                    $of 5
                        <h6>
                            #{translation}
                    $of _
                        <b>
                            #{translation}
            |]
          where
            translation = i18nTranslationOf category

        ------------------------------------------------------------------------
        -- course category div
        ------------------------------------------------------------------------

        {- | Helper function to select if the HTML-header should be added or if
            the content @\<div.courses\>@ tags should nestled deeper.
            
            Group the 'Table' rows into lists of 'Table' rows based on the
            'Category' they belong to. Then loop over all 'Category' based
            lists to generate the different listings.
        -}
        withCat n rows f =
            [shamlet|
                $forall groupedRows <- L.groupBy (catGroup cnf n) rows
                    <div.courses>
                        $maybe category <- catAt cnf n (head groupedRows)
                            #{catHeader n category}
                        #{f groupedRows}
            |]


        ------------------------------------------------------------------------
        -- put everything together
        ------------------------------------------------------------------------

    in [shamlet|
        \<!-- title: #{lookupLang lang $ pageTitle page} -->
        \<!-- fi (Suomenkielinen versio): #{toUrlPath $ lookupLang "fi" $ pageUrl page} -->
        \<!-- se (Svensk version): #{toUrlPath $ lookupLang "se" $ pageUrl page} -->
        \<!-- en (English version): #{toUrlPath $ lookupLang "en" $ pageUrl page} -->
        \

        \<!-- !!! IMPORTANT !!! -->
        \<!-- THIS PAGE IS GENERATED AUTOMATICALLY - DO NOT EDIT DIRECTLY! -->
        \<!-- See https://github.com/SimSaladin/opetussivut for information on how to edit instead -->
        \

        <p>
            $with pg <- head pages
                <a href="#{toUrlPath $ lookupLang lang $ pageUrl pg}">#{lookupLang lang $ pageTitle pg}
            $forall pg <- tail pages
                |
                <a href="#{toUrlPath $ lookupLang lang $ pageUrl pg}">#{lookupLang lang $ pageTitle pg}


        #{markdown def $ LT.fromStrict $ i18nTranslationOf "aputeksti"}


        <div>
            <div.buttons>
                #{i18nTranslationOf "Kieli"}:&nbsp;
                <select id="select-kieli" name="kieli" onchange="updateList(this)">
                    <option value="any">#{i18nTranslationOf "Kaikki"}
                    $forall l <- languages
                        <option value="#{l}">#{i18nTranslationOf l}

                #{i18nTranslationOf "Taso"}:&nbsp;
                <select id="select-taso" name="taso" onchange="updateList(this)">
                    <option value="any">#{i18nTranslationOf "Kaikki"}
                    $forall cat <- (categories !! categoryLevel)
                        <option value="#{cat}">#{i18nTranslationOf cat}

                #{i18nTranslationOf "Lukukausi"}:&nbsp;
                <select id="select-lukukausi" name="lukukausi" onchange="updateList(this)">
                    <option value="any"   >#{i18nTranslationOf "Kaikki"}
                    <option value="syksy" >#{i18nTranslationOf "Syksy"}
                    <option value="kevät" >#{i18nTranslationOf "Kevät"}
                    <option value="kesä"  >#{i18nTranslationOf "Kesä"}

            <div.headers>
                <table style="width:100%">
                    <tr>
                        <td style="width:#{colCodeWidth}">#{i18nTranslationOf colCodeTitle}
                        <td style="width:#{colCourseNameWidth}">#{i18nTranslationOf colCourseNameTitle}
                        <td style="width:#{colPeriodWidth}" >#{i18nTranslationOf colPeriodTitle}
                        <td style="width:#{colRepeatsWidth}" >#{i18nTranslationOf colRepeatsTitle}
                        <td style="width:#{colLangWidth}" >#{i18nTranslationOf colLangTitle}
                        <td style="width:#{colWebsiteWidth}">#{i18nTranslationOf colWebsiteTitle}

            #{courseTable 0 $ tableContent}

        <p>
            #{i18nTranslationOf "Päivitetty"} #{show time}

        <style>
            .buttons {
                padding:1em;
            }

            .headers table {
                width:100%;
                table-layout:fixed;
            }

            .courses table {
                width:100%;
                table-layout:fixed;
            }

            .courses td.compact {
                overflow:hidden;
                text-overflow:ellipsis;
                white-space:nowrap;
            }

            tr[data-pidetaan="next-year"] {
                color:gray;
            }


        <script type="text/javascript">
            #{preEscapedToHtml $ renderJavascript $ jsLogic undefined}
    |]


{- | Creating the javascript functions of the buttons in the HTML files. Select
    only a specific 'Lang'uage, only a specific level etc.
-}
jsLogic :: JavascriptUrl url    -- ^ A link to the different scripts.
jsLogic = [julius|

    window.history.navigationMode = "compatible";

    fs = { };

    updateList = function(e) {
        var name = e.getAttribute("name");
        var opts = e.selectedOptions;

        fs[name] = [];
        for (var i = 0; i < opts.length; i++) {
            fs[name].push(opts[i].getAttribute("value"));
        }

        var xs = document.querySelectorAll(".courses tr");

        for (var i = 0; i < xs.length; i++) {
            xs[i].hidden = !matchesFilters(fs, xs[i]);
        }

        updateHiddenDivs();
    }

    matchesFilters = function(fs, thing) {
        for (var f in fs) {
            if (fs[f] != "any") {
                var m = false;
                for (var i = 0; i < fs[f].length; i++) {
                    if (thing.dataset[f].indexOf(fs[f][i]) > -1) {
                        m = true;
                        break;
                    }
                }
                if (!m) return false;
            }
        }
        return true;
    }

    updateHiddenDivs = function() {
        var xs = document.querySelectorAll(".courses");
        for (var i = 0; i < xs.length; i++) {
            var hidden = true;
            var ts = xs[i].getElementsByTagName("tr");
            for (var j = 0; j < ts.length; j++) {
                if (!ts[j].hidden) {
                    hidden = false;
                    break;
                }
            }
            xs[i].hidden = hidden;
        }
    }

    onload = function(){
        setDefaultSelectedValues();
    }

    setDefaultSelectedValues = function() {
        document.getElementById("select-kieli").value = "any";
        document.getElementById("select-taso").value = "any";
        document.getElementById("select-lukukausi").value = "any";
    }
|]


-- =============================================================================
-- * Get source
-- =============================================================================


{- | This function handles the command line arguments for caching and fetching
    the wiki tables.

    Fetch a confluence doc (wiki page) by id. This function reads the wiki page
    with the given page ID, and parses it as an XML-document. The result is
    cleaned with the @'regexes'@ function.

    It returns the stripped HTML document in XML-format, if it can find the
    currently selected Wiki page, otherwise it returns 'Nothing'.
-}
getData :: String                   -- ^ The page ID.
        -> M (Maybe XML.Document)   -- ^ The cleaned XML-document, if found any.
getData pageId = do
    Config{..} <- ask
    xs         <- lift getArgs
    let file   = cacheDir <> "/" <> pageId <> ".html"

    str <- lift $ case xs of
        "cache" : _ -> Just <$> LT.readFile file
        "fetch" : _ -> do
                        r <- LT.decodeUtf8 <$> simpleHttp (fetchUrl ++ pageId)
                        if "<title>Log In" `LT.isInfixOf` r
                            then do
                                liftIO $ putStrLn (" * Private Wiki Table - Can't read...")
                                return Nothing
                            else do
                                liftIO $ putStrLn (" * Writing to file: " ++ file)
                                LT.writeFile file r >> return (Just r)
        _           -> putStrLn " * Usage: opetussivut <fetch|cache>" >> exitFailure

    return $ cleanAndParse <$> str


{- | This function takes a whole wiki table in 'Text' form and removes some
    standard HTML tags from the text (see @'regexes'@ for more information). It
    then parses an XML document from the HTML body 'Text' stream.

    Uses the @'XML.decodeHtmlEntities'@ setting to decode the 'LT.Text' into
    XML.
-}
cleanAndParse :: LT.Text        -- ^ The raw 'LT.Text' version of the cached wiki page.
              -> XML.Document   -- ^ The XML (HTML) version of the 'LT.Text'.
cleanAndParse = XML.parseText_ parseSettings . LT.pack . foldl1 (.) regexes . LT.unpack
  where
    parseSettings = XML.def { XML.psDecodeEntities = XML.decodeHtmlEntities }


-- =============================================================================
-- * Parse doc
-- =============================================================================


-- | Creates a HTML Table from the cache HTML (in XML format).
parseTable :: XML.Document  -- ^ A 'XML.Document' prepared with the @'cleanAndParse'@ function.
           -> M Table       -- ^ The parsed 'Table'.
parseTable doc = head . catMaybes . findTable (fromDocument doc) <$> ask


{- | Looks for all tables in the generated XML document, with XML-attribute
    /class/ @confluenceTable@, maps the function @'processTable'@ to the result
    list, and finally returns a list containing the discovered 'Table's.
-}
findTable :: Cursor         -- ^ XML document 'Cursor'.
          -> Config         -- ^ Pointer to the /config.yaml/ data.
          -> [Maybe Table]  -- ^ List of 'Maybe' 'Table's 
findTable c cnf = map ($| processTable cnf) (c $.// attributeIs "class" "confluenceTable" :: [Cursor])


{- | This function takes a pointer to the 'Config' data and an XML 'Cursor'
    (pointing to the class attributes with the value @confluenceTable@). It
    picks out all the XML-elements beginning with the tag @tr@ from the
    @confluenceTable@ class 'Cursor' and maps all child elements of the @tr@
    element to it.

    The @cells@ variable contains all the rows of the table. The first row
    contains some higher-level 'Header's (eg. @Vastaava opettaja@), hence this
    row is ignored. The second row contains the main 'Header's of the different
    columns, and the rest of the rows are either 'Course' information or
    'Category's separating the different 'Course' informations.

    The first column in the main 'Header' row is empty (this is the column
    containing the 'Category' headers when looking at the Wiki Table),
    therefore only the 'tail' of it is necessary.
-}
processTable :: Config      -- ^ Pointer to the /config.yaml/ file.
             -> Cursor      -- ^ XML document 'Cursor' pointing at @confluenceTable@ /class/ attribute.
             -> Maybe Table -- ^ The processed 'Maybe' 'Table'.
processTable cnf c = case cells of
    _ : headersRaw : xs ->
        let headers       = tail (mapMaybe getHeader headersRaw)
            (_, mcourses) = L.mapAccumL (getRow cnf headers) [] xs
        in Just $ Table (unsafePerformIO getCurrentTime) headers (catMaybes mcourses)
    _               -> Nothing
  where
    cells = map ($/ anyElement) (c $// element "tr")


{- | Create a 'Maybe' 'Header' type corresponding to the value of the raw
    XML-cell containing information about the 'Header'.
-}
getHeader :: Cursor         -- ^ Pointer to the cell containing information about the 'Header'.
          -> Maybe Header   -- ^ A 'Maybe' 'Header' type corresponding to the 'Cursor' value.
getHeader c = return . T.toLower . normalize $ T.unwords (c $// content)


{- | A row is either a 'Category' or a 'Course'. The @['Category']@ is used as
    an accumulator.
-}
getRow :: Config                        -- ^ Pointer to the 'Config' for use with the @'toCategory'@ function.
       -> [Header]                      -- ^ List of the 'Table' 'Header's.
       -> [Category]                    -- ^ A list of 'Category' objects associated with this 'Course'.
       -> [Cursor]                      -- ^ The list of unprocessed rows in the 'Table'.
       -> ([Category], Maybe Course)    -- ^ A single row, containing 'Category's and the 'Course' data (if any).
getRow cnf@Config{..} headers cats cs = map (T.unwords . ($// content)) cs `go` head (cs !! 1 $| attribute "class")
  where
    go []        _       = (cats, Nothing)
    go (mc : vs) classes = case toCategory cnf mc of
            Just cat                        -> (accumCategory cnf cat cats, Nothing)
            Nothing | null vs               -> (cats, Nothing)
                    | T.null (normalize mc) -> (cats, Just $ toCourse cnf cats headers (classCur `T.isInfixOf` classes) vs)
                    | otherwise             -> (cats, Just $ toCourse cnf cats headers (classCur `T.isInfixOf` classes) vs)


-- =============================================================================
-- ** Courses and categories
-- =============================================================================


{- | Checks if the given 'Text' is a 'Category' listed in the /config.yaml/
    file. If it is, then it will return the name of the 'Category' otherwise
    it'll return an empty 'Text'.

    Because the Wiki Table column containing the categories also contains
    semester information or empty cells, the function has to exclude those
    cells before checking the 'Category' name.
-}
toCategory :: Config            -- ^ Pointer to the 'Config' object, for accessing the @categories@.
           -> Text              -- ^ The cell value from the 'Table'.
           -> Maybe Category    -- ^ The name of the 'Category' if it is a category, otherwise an empty 'Text'.
toCategory Config{..} t = do
    guard $ t /= "\160" && t /= "syksy" && t /= "kevät"
    guard $ isJust $ L.find (`T.isInfixOf` (T.toLower t)) $ concat categories
    return $ normalize $ T.toLower t


{- | Accumulate a 'Category' to a list of 'Category's based on what @categories@
    cannot overlap.

    In the /config.yaml/ file the @categories@ are listed in hierarchial order,
    making the once from the top being on the top if more than one 'Category' is
    found for that particular 'Course'.

    If the 'Category' to check can be found in the list of @categories@, it will
    grab the index in the list of @categories@ for that 'Category' and generate
    the new list of 'Category's for the 'Course'.
-}
accumCategory :: Config         -- ^ Pointer to the 'Config' for access to the @categories@.
              -> Category       -- ^ The current 'Category' to check.
              -> [Category]     -- ^ The previous 'Category's for the course.
              -> [Category]     -- ^ The list containing all 'Category's found for the course this far in the right order.
accumCategory Config{..} cat cats = case L.findIndex (any (`T.isPrefixOf` cat)) categories of
    Nothing -> error $ "Unknown category: " ++ show cat
    Just i  -> L.deleteFirstsBy T.isPrefixOf cats (f i) ++ [cat]
  where
    f i = concat $ L.drop i categories


{- | Creates a row for the current 'Table'. The output will differ depending on
    the content in the 'Config' data and the different arguments.

    This function will return the finished row, containing the separating
    'Category's and the correct 'Course' information from the source 'Table'.
-}
toCourse :: Config          -- ^ The 'Config' to lookup page configuration data from.
         -> [Category]      -- ^ A list of 'Category's to pass on to the finished row.
         -> [Header]        -- ^ A list of 'Header's to select correct 'Text' from the given list.
         -> Bool            -- ^ If 'True' this will make the course available this year.
         -> [Text]          -- ^ Used to fill the columns of the row with values from the source 'Table'.
         -> Course          -- ^ The finished row.
toCourse Config{..} cats hs iscur xs =
    (cats, Map.adjust doLang (columnTitle columnHeaders "colLang") $
           Map.adjust doRepeats (columnTitle columnHeaders "colRepeats") $
           Map.insert "pidetään" (if iscur then "this-year" else "next-year") $
           Map.insert "lukukausi" lukukausi vals)
  where
    vals      = Map.fromList $ zip hs $ map normalize xs
    lukukausi = fromMaybe "syksy, kevät" $ Map.lookup (columnTitle columnHeaders "colPeriod") vals >>= toLukukausi
    toLukukausi x
        | "tammikuu"  `T.isInfixOf` x                                    = Just "kevät"
        | "helmikuu"  `T.isInfixOf` x                                    = Just "kevät"
        | "maaliskuu" `T.isInfixOf` x                                    = Just "kevät"
        | "huhtikuu"  `T.isInfixOf` x                                    = Just "kevät"
        | "toukokuu"  `T.isInfixOf` x                                    = Just "kevät"
        | "kesäkuu"   `T.isInfixOf` x                                    = Just "kesä"
        | "heinäkuu"  `T.isInfixOf` x                                    = Just "kesä"
        | "elokuu"    `T.isInfixOf` x                                    = Just "kesä, syksy"
        | "syyskuu"   `T.isInfixOf` x                                    = Just "syksy"
        | "lokakuu"   `T.isInfixOf` x                                    = Just "syksy"
        | "marraskuu" `T.isInfixOf` x                                    = Just "syksy"
        | "joulukuu"  `T.isInfixOf` x                                    = Just "syksy"
        | x == "I"    || x == "II"     || x == "I-II"                    = Just "syksy"
        | x == "III"  || x == "IV"     || x == "III-IV"                  = Just "kevät"
        | x == "I-IV" || x == "II-III" || x == "I, III" || x == "II, IV" = Just "syksy, kevät"
        | x == "V"                                                       = Just "kesä"
        | "kevät" `T.isInfixOf` x                                        = Just "kevät"
        | "syksy" `T.isInfixOf` x                                        = Just "syksy"
        | "kesä"  `T.isInfixOf` x                                        = Just "kesä"
        | otherwise                                                      = Nothing


{- | Change the format of the @colLang@ column in the source 'Table' to be in
    the correct format.

        * Replace different ways of writing languages to the correct 'Lang'
          format.

        * Replace word separators to single spaces.
-}
doLang :: Text      -- ^ The line of 'Text', that needs conversion.
       -> Text      -- ^ The correctly formatted 'Text'.
doLang = T.replace "suomi" "fi" . T.replace "eng" "en" . T.replace "englanti" "en"
       . T.replace "ruotsi" "se"
       . T.unwords . T.words
       . T.replace "," " " . T.replace "." " " . T.replace "/" " " . T.toLower


{- | Checks the column @toistuu@ from the source 'Table'. If there's anything
    but numericals or non-alpha characters, it'll return a 'Text' consisting
    of the '-' character. Else it returns the value of the cell.
-}
doRepeats :: Text   -- ^ The 'Text' in the cell of the column.
          -> Text   -- ^ The value of 'Text' argument if there isn't any alpha characters in the cell.
doRepeats x | T.any isLetter x = "-"
            | otherwise        = x


{- | The 'Course' in this case is a row in the 'Table'. This function compares
    two rows and checks if the values are found in the @categories@ list.
    It will compare the 'Text's of them and returns 'True' if both of the
    applied 'Course' arguments have the same text.
-}
catGroup :: Config      -- ^ Used to access all available @categories@ from /config.yaml/.
         -> Int         -- ^ 'Category' at level @n@ in /config.yaml/.
         -> Course      -- ^ First 'Table' row to compare.
         -> Course      -- ^ Second 'Table' row to compare.
         -> Bool        -- ^ 'True' if the two rows have the same 'Category' 'Text'.
catGroup cnf n = (==) `on` catAt cnf n


{- | Returns the value of the first found 'Category' that matches the
    @categories@ at level @n@ in /config.yaml/.
-}
catAt :: Config         -- ^ Used to access all available @categories@ from /config.yaml/.
      -> Int            -- ^ 'Category' at level @n@ in /config.yaml/.
      -> Course         -- ^ A 'Table' row consisting of only a 'Category' 'Text'.
      -> Maybe Text     -- ^ The first found value matching @categories@ at level @n@.
catAt Config{..} n (cats, _) =
    case [ c | c <- cats, cr <- categories !! n, cr `T.isPrefixOf` c ] of
        x:_ -> Just x
        _   -> Nothing


{- | Get the content of a specific cell from the specified row. This function
    works as a wrapper for the @'getCellContentMaybe'@ function, stripping it
    of the 'Maybe' monad.

    If the @'getCellContentMaybe'@ returns a 'Nothing' it'll return a 'Text'
    saying that the /key couldn't be found/, otherwise it returns the value
    of the cell.
-}
getCellContent :: Text      -- ^ The 'Text' to look for in the row consisting of the 'Course' type.
               -> Course    -- ^ The row to look in.
               -> Text      -- ^ The result 'Text'.
getCellContent k c = fromMaybe (traceShow ("Key not found" :: String, k, c) $ "Key not found: " <> k) $ getCellContentMaybe k c


{- | Get the content of a specific cell form the specified row.

    Returns 'Nothing' if the key isn't in the row, otherwise it returns the
    value of the cell.
-}
getCellContentMaybe :: Text           -- ^ The 'Text' to look for in the row.
                    -> Course         -- ^ The row to look in.
                    -> Maybe Text     -- ^ The 'Text' of the cell if it was found otherwise 'Nothing'.
getCellContentMaybe k (_, c) = Map.lookup k c



