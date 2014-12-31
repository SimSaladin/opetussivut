# opetussivut

    opetussivut <fetch|cache>

To build pages from cached pages, do `opetussivut cache`. Or to fetch from wiki
do `opetussivut fetch`.

## Automatic execution

Use a cron entry, like

    7 0 * * *  /usr/local/share/opetussivut/opetussivut fetch

## Configuration

Configuration is read from `./config.yaml`. See that file for configurable
parameters.

## Changing the layout

If you want to change the output HTML, see `tableBody` function in `main.hs`.
It's hamlet syntax: just omit any end tags and indicate nesting with
indendation. Editing `main.hs` requires recompilation: `cabal install` in its
containing directory (assumes GHC and cabal-install installed). The new binary
is then found in `~/.cabal/bin/opetussivut`, from where you can copy it to the
deployment server.


