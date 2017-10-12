#!/bin/bash
# stop when error
set -e

echo testing backend
echo
stack test --ghc-options -O2 --ghc-options -threaded
echo 