#!/bin/bash
# stop when error
set -e

echo
echo testing backend
stack test --ghc-options -O2 --ghc-options -threaded

echo
echo try to initialze the database by use sql files
sudo psql -U postgres -f sql/initialization.sql