
Source: <https://github.com/SimSaladin/opetussivut>

# User guide

This is meant to be completely automated system that generates the web pages
from corresponding wiki pages. The pages are updated at midnight if edits
were made to corresponding wiki page since last update.

As a user (wiki editor), you just edit the wiki pages as before.
(Note: we assume [some of the column names](https://github.com/SimSaladin/opetussivut/blob/master/config.yaml#L327-L333).)
Generation of the HTML pages is controlled via the visibility setting of the wiki page:

- Set the wiki page as *publicly readable* and the website will be updated
  within 24 hours.
- Set the wiki page as *not* publicly readable and the pages will not be
  generated again until you set the page publicly readable.

Setting the wiki page private can be useful when doing big changes over a longer
time period.

---

If you add something that should be translated to a wiki page in its generated
html, then add the translations to the `i18n` section in `config.yaml` on the
server (the default location of the configuration is
`/usr/local/share/opetussivut/config.yaml`). See that section for
[examples](https://github.com/SimSaladin/opetussivut/blob/master/config.yaml#L116).

---

If you need to change the wiki page used for some of the listings, then add swap
the new wiki page ID in `config.yaml`. The ID is found as follows: go to the
wiki page in your browser and choose **Tools > Page Information**. Now the URL bar
should look like

    https://wiki.helsinki.fi/pages/viewinfo.action?pageId=100000000
                                                          ^^^^^^^^^

where the ID is above the `^`'s. Now swap that with the page id you want to
replace in `config.yaml`.

Similarly, if you want to add a completely new page, add a corresponding
`(pageId, pageTitle, pageUrl)`-block in the `config.yaml`. You can test the
html generation by running `./wrapper.sh fetch` in the same directory as
`config.yaml`.

# Technical documentation

(When reinstalling or otherwise tweaking)

Usage:

    opetussivut <fetch|cache>

To fetch newest listing from wiki and build the pages (and cache results), do
`opetussivut fetch`.

Or to rebuild the pages from the cache, do `opetussivut cache`.

When fetching from wiki, we ignore the result if the page is **hidden** from
public. (The cache is *not* then updated nor the pages built.)

In deployment, we use a `wrapper.sh` script over the `opetussivut`-binary. The
wrapper executes the binary in right working directory and provides it correct
dynamic libraries (`libgmp` in ./lib). All this must be in the same directory,
i.e. so that there is a directory that contains files `wrapper.sh`, opetussivut
binary as `update`, `config.yaml`, and a directory named `sivut`, and that the
directories are writable by the user that executes the wrapper.

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
indendation. Editing `main.hs` requires recompilation: `cabal install`
in its containing directory (assumes GHC and cabal-install installed). The new
binary is then found in `~/.cabal/bin/opetussivut`, from where you can copy it
to the deployment server.


