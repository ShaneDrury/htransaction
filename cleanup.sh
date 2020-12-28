#!/bin/sh

find app test src -type f -name "*.hs" | xargs -Ixx ormolu --ghc-opt -XTypeApplications -m inplace xx && find app test src -type f -name "*.hs" | xargs -Ixx hlint --refactor --refactor-options="-i" xx
