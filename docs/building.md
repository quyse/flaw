# Build machine preparation

[ghc binaries built with integer-simple](https://drive.google.com/drive/folders/0BwTCpCPC0rtuaDduUG5zd0t0VWM?usp=sharing)

## Linux

Simply use [the docker image](https://github.com/quyse/haskell-hacks/tree/master/haskell-build-ghcjs-runner).

## Windows

* Install `git`, `stack`, [OpenAL SDK](https://www.openal.org/downloads/OpenAL11CoreSDK.zip), [Vulkan SDK](https://vulkan.lunarg.com/sdk/home#windows).
* Install `integer-simple` ghc.
* Copy OpenAL and Vulkan libs into ghc's `mingw/lib` path.

## macOS

Install Homebrew.

Run `brew install pkg-config sdl2 ffmpeg`.

## Global config.yaml

```yaml
ghc-options:
  "*": -O3 -threaded -with-rtsopts=-N -Wall -fno-warn-unsupported-calling-conventions -fno-warn-tabs -fexternal-interpreter

rebuild-ghc-options: true
apply-ghc-options: everything

dump-logs: all

build:
  split-objs: true
```

`-fexternal-interpreter` is used only on Windows (fixes problem with linking C++). On Linux it causes link errors.

`split-objs` are not supported on macOS (build fails).
