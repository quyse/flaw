# FLAW Engine

FLAW is a highly experimental game engine written in Haskell. In very early stage. Heavily based on my another work [Inanity](https://github.com/quyse/inanity). The goal is to discover new horizons for using Haskell in game development.

## Planned Features

* Cross-platform (Windows, Linux at least)
* Abstracted graphics API (baked by DirectX 11/OpenGL)
* Cross-API Haskell inline shaders (similar to Inanity's C++ inline shaders)
* Low-level Haskell FFI bindings to DirectX 11 and OpenGL
* Math library generated with Template Haskell
* Multi-thread rendering
* TH-based game build, type-safe references to game assets
* FRP-style GUI framework
* TH-generated bindings for scripting languages
* Asset Editor based on ideas of [Inanity Oil](https://github.com/quyse/oil)
