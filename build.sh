#!/usr/bin/env bash
mkdir -p dist
ghc Ast.hs
ghc AstContext.hs
ghc -o dist/main -odir dist/ -hidir dist/ Main.hs -rtsopts
if [[ $1 == "run" ]]; then
  ./dist/main +RTS -cx
fi
