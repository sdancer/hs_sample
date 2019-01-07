#!/usr/bin/env bash
mkdir -p dist
ghc -o dist/main -odir dist/ -hidir dist/ Main.hs
