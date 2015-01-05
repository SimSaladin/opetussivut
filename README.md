
Source: <https://github.com/SimSaladin/opetussivut>

# Usage scenarios

This is meant to be completely automated system that generates the web pages
from corresponding wiki pages. The pages are updated at midnight if edits
were made to corresponding wiki page since last update.

As a user (wiki editor), you just edit the wiki pages as before. Generation of
the HTML pages is controlled via the visibility setting of the wiki page:

- Set the wiki page as *publicly readable* and the website will be updated
  within 24 hours.
- Set the wiki page as *not* publicly readable and the pages will not be
  generated again until you set the page public again.

Setting the wiki page private can be useful when doing big changes over a longer
time period.

# Technical documentation

(When reinstalling or otherwise tweaking)

Usage:

    opetussivut <fetch|cache>

To fetch newest listing from wiki and build the pages (and cache results), do
`opetussivut fetch`.

Or to rebuild the pages from the cache, do `opetussivut cache`.

When fetching from wiki, we ignore the result if the page is **hidden** from
public. (The cache is *not* then updated nor the pages built.)

## Automatic execution

Use a cron entry, like

    # ignores warnings from stdin; real errors get printed to stderr
    7 0 * * *  /usr/local/share/opetussivut/wrapper.sh fetch >&-

## Configuration

Configuration is read from `./config.yaml`. See that file for configurable
parameters.

## Changing the HTML layout

If you want to change the output HTML, see `tableBody` function in `main.hs`.
It's *hamlet* syntax: just omit any end tags and indicate nesting with
indendation. Editing `main.hs` requires recompilation: `cabal install -fstatic`
in its containing directory (assumes GHC and cabal-install installed). The new
binary is then found in `~/.cabal/bin/opetussivut`, from where you can copy it
to the deployment server.


