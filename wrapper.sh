#!/bin/sh

# Wrap ./update with custom libs in ./lib,
# where . is the dirname of this script.

cd $(dirname $0)

LD_LIBRARY_PATH=./lib ./update $1
