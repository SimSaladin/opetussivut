
Source <https://github.com/SimSaladin/opetussivut>

# Usage scenarios

This is meant to be completely automated system that generates the web pages
from corresponding wiki pages. The listings are updated every day at 0.07, or
when new content is available.

As a user, you just need to edit the wiki pages and **if the page is publicly
readable** then the website pages will be updated within 24 hours. **Set the
page as private (not publicly readable)** and the pages will not be generated
again until you set the page public again. Setting the wiki page private can be
useful when doing big changes over a longer time period.

# Technical documentation

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

## Changing the layout

If you want to change the output HTML, see `tableBody` function in `main.hs`.
It's *hamlet* syntax: just omit any end tags and indicate nesting with
indendation. Editing `main.hs` requires recompilation: `cabal install -fstatic`
in its containing directory (assumes GHC and cabal-install installed). The new
binary is then found in `~/.cabal/bin/opetussivut`, from where you can copy it
to the deployment server.


