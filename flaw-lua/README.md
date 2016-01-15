# flaw-lua

`flaw-lua` is an experimental implementation of [Lua programming language](http://www.lua.org/) in Haskell. More precisely, it's a translator from Lua bytecode into Haskell code. The translator works as a Template Haskell splice or quasiquoter, hence embedding translated code right into Haskell program during normal compilation.

`flaw-lua` doesn't include its own lexer/parser, it looks only at Lua bytecode. In order to compile Lua source into Lua bytecode `flaw-lua` uses reference Lua library from [lua.org](http://www.lua.org).

This experiment was inspired by [Luerl](https://github.com/rvirding/luerl) - Lua implementation in Erlang, and adheres similar goals:

* make possible to use lightweight threads, advanced concurrency and parallelism provided by Haskell runtime in Lua
* make it easier to embed Lua programs into Haskell programs

The implementation is very experimental and by no means complete.

## Deliberate deviations

* There's only one type of `userdata` (instead of light userdata and full userdata in original Lua), and it cannot have metatable. Instead it contains a single Haskell value. The plan is to allow overriding standard operators by instantiating special typeclass instead of using metatable. That way it should work faster and look more haskell-ish.

## Limitations

* No support for `load` standard function. As there's no interpreter, all chunks must be known at compile time. However, to make it easier to run existing Lua code, `loadfile` is implemented via usage of pre-embedded functions from environment: it simply returns `_G._chunks[filename]`. That way you can compile and embed all chunks into Haskell program and put them into `_chunks` table, therefore existing Lua code calling `loadfile` will work without modifications. `dofile` calls `loadfile`, so it works too.
* No support for dynamic number of results returned from a function or by `return` statement. (For those familiar with Lua internals, no support for `stack top` value). This arises in innocent code like `f(g())` where all results from `g` invocation (whatever their number is) should be provided to `f`. It's just not implemented yet.

## How it works

Translation is quite straightforward.

* Every Lua function becomes a Haskell function running in normal `IO` monad.
* Locally defined Lua functions become locally `let`-defined Haskell functions, and Lua closures are real Haskell closures, so no special machinery for keeping closure's upvalues needed.
* Lua's mutable stack is represented by number of `IORef LuaValue` things created on start of every function.
* Type of every Lua function in Haskell is `[LuaValue] -> IO [LuaValue]`.
* Type of every Lua chunk is `IORef LuaValue -> [LuaValue] -> IO [LuaValue]`, so it's basically a Lua function additionally accepting Lua environment as a first argument. All functions in chunk inherit that environment.
