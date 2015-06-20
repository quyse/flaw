# FLAW Engine

FLAW is a highly experimental game engine written in Haskell. In very early stage. Heavily based on my another work [Inanity](https://github.com/quyse/inanity). The goal is to discover new horizons for using Haskell in game development.

## What works

* Platform support: Windows, web (via GHCJS)
* Abstracted graphics API: DirectX 11, WebGL
* Cross-API Haskell inline shaders (similar to Inanity's C++ inline shaders)
* Math library generated with Template Haskell
* TH-based game build, type-safe game assets
* Low-level user input
* Simple windowing

## Planned

* Platform support: Linux
* Abstracted graphics API: OpenGL

## Dreams

* FRP-style GUI framework
* TH-generated bindings for scripting languages
* Type-safe multiplayer framework
* Asset Editor based on ideas of [Inanity Oil](https://github.com/quyse/oil)

## Demo

Simple stupid WebGL game made for Ludum Dare 32: [Peka vs Beaver](http://ludumdare.com/compo/ludum-dare-32/?action=preview&uid=49212).
