#!/usr/bin/env bash
mkdir -p dist
ghc -o dist/main -odir dist/ -hidir dist/ Main.hs -prof -fprof-auto -fprof-cafs -rtsopts
if [[ $1 == "run" ]]; then
  ./dist/main +RTS -cx
fi
