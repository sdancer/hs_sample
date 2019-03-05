# Retoolkit
another reverse engineering toolkit, writen in haskell
not much to say yet

## Project Goals
* quereable
* equation system
* symbolic execution
* compile IR back to native code
* transformations (instrumentation)
* decompiler backend

## Setup and Build Instructions
This project depends on Capstone version 3.0.4 and Hapstone version 0.3.0.0, the Haskell bindings for Capstone. Note that Hapstone version 0.3.0.0 is not on Hackage. Therefore, follow the following instructions to setup a development environment for Retoolkit:
1. `git clone "https://github.com/murisi/hs_sample"` to obtain the Retoolkit package
2. `git clone "https://github.com/ibabushkin/hapstone"` to obtain Hapstone
3. `cd hs_sample`
4. `cabal sandbox init` to ensure that the following installations do not have global effects
5. `cabal install c2hs` to install c2hs, a dependency of Hapstone
6. `cabal sandbox add-source ../hapstone/` to show Cabal where to find the Hapstone dependency
7. `cabal install --only-dependencies` to install Hapstone in this sandbox
8. `cabal configure`
9. `cabal build` to (re)build the binaries whenever the Haskell source is changed

