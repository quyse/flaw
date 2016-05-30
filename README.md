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
* Tools

## Experiments

* TH-based Lua-to-Haskell compiler.

## Dreams

* TH-generated bindings for scripting languages
* Type-safe multiplayer framework
* Real-time collaborative asset editor
