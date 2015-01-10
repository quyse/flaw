# FLAW Engine

FLAW is a highly experimental game engine written in Haskell. In very early stage.

# Building

* Make sandbox
```bash
cabal sandbox init
```
* Add everything as a source.
```bash
cabal sandbox add-source ../flaw/flaw-base
cabal sandbox add-source ../flaw/flaw-ffi
...
```
* Build.

## Other things

### `network` package on Windows

To install run this in cygwin:
```bash
PATH="/cygdrive/c/Program Files/Haskell Platform/2014.2.0.0/mingw/bin:$PATH" cabal install network --configure-option --build=x86_64-w64-mingw32
```
