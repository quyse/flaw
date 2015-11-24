# FLAW Engine

[![Build Status](https://travis-ci.org/quyse/flaw.svg?branch=master)](https://travis-ci.org/quyse/flaw)

FLAW is a highly experimental game engine written in Haskell. In very early stage. Heavily based on my another work [Inanity](https://github.com/quyse/inanity). The goal is to discover new horizons for using Haskell in game development.

## What works

* Platform support: Windows, Linux, web (via GHCJS)
* Abstracted graphics API: DirectX 11, OpenGL, WebGL
* Cross-API Haskell inline shaders (similar to Inanity's C++ inline shaders) translated to HLSL and GLSL
* TH-generated math library
* TH-based game build, type-safe game assets
* Low-level user input (Raw Input on Windows, SDL on Linux)
* Simple windowing
* FreeType and Harfbuzz integration, antialiased text rendering
* Collada import (rigid and skinned geometries, animations)

## Planned

* GUI framework.

## Dreams

* TH-generated bindings for scripting languages
* Type-safe multiplayer framework
* Asset Editor based on ideas of [Inanity Oil](https://github.com/quyse/oil)

## Demo

Simple stupid WebGL game made for Ludum Dare 32: [Peka vs Beaver](http://ludumdare.com/compo/ludum-dare-32/?action=preview&uid=49212).
