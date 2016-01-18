# FLAW Engine

[![Build Status](https://travis-ci.org/quyse/flaw.svg?branch=master)](https://travis-ci.org/quyse/flaw)
[![Windows Build Status](https://ci.appveyor.com/api/projects/status/u8p499pkcvkavwq0/branch/master?svg=true)](https://ci.appveyor.com/project/quyse/flaw)

FLAW is a highly experimental game engine written in Haskell. In very early stage. Heavily based on my another work [Inanity](https://github.com/quyse/inanity). The goal is to discover new horizons for using Haskell in game development.

## What works

* Platform support: Windows, Linux, web (via GHCJS)
* Abstracted graphics API: DirectX 11, OpenGL, WebGL
* Abstracted inline shaders in Haskell, with HLSL and GLSL backends
* TH-generated math library, with efficient unboxed fixed-length vectors/matrices
* TH-based game build, type-safe game assets
* Low-level user input library (via Raw Input on Windows, SDL on Linux, browser DOM APIs on Web)
* Simple windowing API
* Fast text rendering with subpixel anti-aliasing
* Collada import (rigid and skinned geometries, animations)

## In progress

* High-level GUI framework made from scratch (i.e. neither using native GUIs nor frameworks like Qt)

## Experiments

* TH-based Lua-to-Haskell compiler.

## Dreams

* TH-generated bindings for scripting languages
* Type-safe multiplayer framework
* Real-time collaborative asset editor

## Demo

Simple stupid WebGL game made for Ludum Dare 32: [Peka vs Beaver](http://ludumdare.com/compo/ludum-dare-32/?action=preview&uid=49212).
