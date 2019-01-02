mkdir -p dist
ghc -o dist/real_code -odir dist/ -hidir dist/ real_code.hs
