#!/bin/bash

#---------------------------------------------------------
# Removing:
#     dist directory
#     webapp directory
#     and temporary files ending with '~' in whole root
#---------------------------------------------------------

rm -r -v dist
rm -r -v webapp
find . -name \*~ -exec rm -v {} +
