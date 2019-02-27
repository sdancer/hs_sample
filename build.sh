#!/usr/bin/env bash
mkdir -p dist
ghc -odir dist/ -hidir dist/ Ast.hs
ghc -odir dist/ -hidir dist/ AstContext.hs
ghc -odir dist/ -hidir dist/ X86Sem.hs
ghc -odir dist/ -hidir dist/ test/test1.hs
ghc -o dist/main -odir dist/ -hidir dist/ Main.hs -rtsopts
if [[ $1 == "run" ]]; then
  ./dist/main +RTS -cx
fi
