# FLAW Engine

[![Gitlab Build Status](https://gitlab.com/quyse/flaw/badges/master/build.svg)](https://gitlab.com/quyse/flaw/builds)
[![Code Climate](https://codeclimate.com/github/quyse/flaw/badges/gpa.svg)](https://codeclimate.com/github/quyse/flaw)
[![Haddock Status](https://quyse.gitlab.io/flaw/docs/badge.svg)](https://quyse.gitlab.io/flaw/docs/)
[![Test Coverage Status](https://quyse.gitlab.io/flaw/hpc/badge.svg)](https://quyse.gitlab.io/flaw/hpc/)

FLAW is a highly experimental game engine written in Haskell.

## Motivation

FLAW is heavily based on my experiences from developing [Inanity engine](https://github.com/quyse/inanity). In short, the experiences are: C++ sucks, game development can be done better. It's time to discover new horizons by using Haskell in the area where it hasn't been used a lot (yet).

This project is mostly a playground for absolutely crazy ideas. Surely if someone managed to make a real game out of it, that game would be absolutely crazy too!

## What works

* Platform support: Windows, Linux, OS X, web (via GHCJS)
* Abstracted graphics API: DirectX 11, OpenGL, WebGL
* Abstracted inline shaders in Haskell, with HLSL and GLSL backends
* TH-generated math library, with efficient unboxed fixed-length vectors/matrices
* TH-based game build, type-safe game assets
* Low-level user input library (via Raw Input on Windows, SDL on Linux and OS X, browser DOM APIs on Web)
* Simple windowing API
* Fast text rendering with subpixel anti-aliasing
* Collada import (rigid and skinned geometries, animations)

## In progress

* Visual effects (deferred rendering, materials, lighting, shadowing, etc)
* High-level GUI framework made from scratch (i.e. neither using native GUIs nor frameworks like Qt)
* Real-time collaborative asset editor ([doc](docs/editor.md))

## Experiments

* TH-based Lua-to-Haskell compiler.

## Dreams

* TH-generated bindings for scripting languages
* Type-safe multiplayer framework

## Packages and platform support

FLAW has moved to GHC 8. A few packages are not compilable currently due to not updated dependencies.

Here are all packages in A-Z order. Checkmark against a platform means that the package is being actually built and tested (if it has tests :D) on this platform during CI.

Linux is the main development platform. It also considered the only "server" platform, hence several "server" packages are tested only on Linux, even if it's probably possible to compile and run them on Windows or OS X.

| Package | Description | Linux | Windows | OS X | Web (GHCJS) |
|---|---|:---:|:---:|:---:|:---:|
| `flaw-al` | OpenAL integration - implementation of `flaw-audio` interface | ✔ | ✔ | ✔ |   |
| `flaw-app` | Handy engine initialization and main loop routines | ✔ | ✔ | ✔ | ✔ |
| `flaw-asset` | Packing and unpacking assets into asset packs | ✔ | ✔ | ✔ | ✔ |
| `flaw-asset-dxt` | DXT image compression based on [fork](https://gitlab.com/quyse/libsquish) of [libsquish](https://code.google.com/p/libsquish/) library | ✔ | ✔ | ✔ |   |
| `flaw-asset-wai` | Asset distribution for web games with perfect caching | ✔ |   |   |   |
| `flaw-audio` | General interface for audio device | ✔ | ✔ | ✔ | ✔ |
| `flaw-base` | Base primitives (resource management, concurrency, etc) | ✔ | ✔ | ✔ | ✔ |
| `flaw-build` | Embedding data into executable | ✔ | ✔ | ✔ | ✔ |
| `flaw-bullet` | Initial integration with Bullet Physics engine | ✔ | ✔ | ✔ |   |
| `flaw-canvas` | Very simple 2D drawing | ✔ | ✔ | ✔ | ✔ |
| `flaw-collada` | Importing Collada assets | ✔ | ✔ | ✔ |   |
| `flaw-dx11` | DirectX 11 integration - implementation of `flaw-graphics` interface |   | ✔ |   |   |
| `flaw-editor` | Visual asset editor (WIP) | ✔ | ✔ | ✔ |   |
| `flaw-ffi` | Interoperation with native code | ✔ | ✔ | ✔ | ✔ |
| `flaw-ffmpeg` | Experimental bindings to `ffmpeg` | ✔ |   | ✔ |   |
| `flaw-font` | General interface for font rendering | ✔ | ✔ | ✔ |   |
| `flaw-font-fthb` | Implementation of `flaw-font` interface using FreeType 2 and Harfbuzz | ✔ | ✔ | ✔ |   |
| `flaw-game` | Non-finished experimental stuff about game synchronization | ✔ | ✔ | ✔ | ✔ |
| `flaw-gl` | OpenGL integration - implementation of `flaw-graphics` interface | ✔ | ✔ | ✔ | ✔ |
| `flaw-gl-mesa` | Experimental MESA integration for offscreen software rendering | ✔ |   |   |   |
| `flaw-gl-sdl` | OpenGL-to-windowing binding via SDL | ✔ |   | ✔ |   |
| `flaw-gl-win32` | OpenGL-to-windowing binding via Win32 API |   | ✔ |   |   |
| `flaw-graphics` | General interface for graphics | ✔ | ✔ | ✔ | ✔ |
| `flaw-input` | General user input interface | ✔ | ✔ | ✔ | ✔ |
| `flaw-js` | Some generic javascript routines |   |   |   | ✔ |
| `flaw-lua` | Experimental Lua-to-Haskell translator | ✔ | ✔ | ✔ |   |
| `flaw-lua-refimpl` | Lua reference implementation from lua.org | ✔ | ✔ | ✔ |   |
| `flaw-math` | Math library | ✔ | ✔ | ✔ | ✔ |
| `flaw-network` | Some network socket implementations (WIP) | ✔ | ✔ | ✔ | ✔ |
| `flaw-oil` | Reimplementation of [Oil protocol](https://github.com/quyse/oil) | ✔ | ✔ | ✔ |   |
| `flaw-oil-client` | Simple command-line client for Oil protocol | ✔ | ✔ | ✔ |   |
| `flaw-oil-entity` | Editable entities over Oil protocol | ✔ | ✔ | ✔ |   |
| `flaw-oil-server` | Server for Oil protocol | ✔ |   |   |   |
| `flaw-physics` | General interface for physics simulation | ✔ | ✔ | ✔ | ✔ |
| `flaw-sdl` | Integration with SDL 2 | ✔ |   | ✔ |   |
| `flaw-sl` | Generic shader languages helping code | ✔ | ✔ | ✔ | ✔ |
| `flaw-social` | Some social network integration | ✔ |   |   | ✔ |
| `flaw-sqlite` | SQLite binding | ✔ | ✔ | ✔ |   |
| `flaw-ui` | UI library using `flaw-graphics` for rendering | ✔ | ✔ | ✔ |   |
| `flaw-ui-default-style` | Hardcoded "default" UI styles | ✔ | ✔ | ✔ |   |
| `flaw-visual` | High-level rendering techniques | ✔ | ✔ | ✔ |   |
| `flaw-vulkan` | Vulkan integration - implementation of `flaw-graphics` interface | ✔ |   |   |   |
| `flaw-websocket-client` | Websocket client library |   |   |   | ✔ |
| `flaw-websocket-server` | Websocket server library | ✔ |   |   |   |
| `flaw-window` | Windowing platform bindings and graphics support | ✔ | ✔ | ✔ | ✔ |
