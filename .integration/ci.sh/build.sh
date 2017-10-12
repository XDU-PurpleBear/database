#!/bin/bash
# stop when error
set -e

stack build --ghc-options -O2 --ghc-options -threaded