
Source <https://github.com/SimSaladin/opetussivut>

    opetussivut <fetch|cache>

To fetch newest listing from wiki and build the pages (and cache results), do
`opetussivut fetch`.

Or to rebuild the pages from the cache, do `opetussivut cache`.

When fetching from wiki, we ignore the result if the page is **hidden** from
public. (The cache is *not* then updated nor the pages built.)

## Automatic execution

Use a cron entry, like

    # ignores warnings from stdin; real errors get printed to stderr
    7 0 * * *  /usr/local/share/opetussivut/opetussivut fetch >&-

## Configuration

Configuration is read from `./config.yaml`. See that file for configurable
parameters.

## Changing the layout

If you want to change the output HTML, see `tableBody` function in `main.hs`.
It's *hamlet* syntax: just omit any end tags and indicate nesting with
indendation. Editing `main.hs` requires recompilation: `cabal install -fstatic`
in its containing directory (assumes GHC and cabal-install installed). The new
binary is then found in `~/.cabal/bin/opetussivut`, from where you can copy it
to the deployment server.


