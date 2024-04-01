# HsLua – Bindings to Lua, an embeddable scripting language

[![Build status][GitHub Actions badge]][GitHub Actions]
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Hackage]](https://hackage.haskell.org/package/hslua)

HsLua provides bindings, wrappers, types, and helper functions to bridge
Haskell and Lua.

[GitHub Actions badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[GitHub Actions]: https://github.com/hslua/hslua/actions
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/main?svg=true
[Hackage]: https://img.shields.io/hackage/v/hslua.svg

This package is part of [HsLua], a Haskell framework built around
the embeddable scripting language [Lua].

[HsLua]: https://hslua.org/
[Lua]: https://lua.org/


Overview
--------

[Lua](https://lua.org) is a small, well-designed, embeddable scripting language.
It has become the de-facto default to make programs extensible and is widely
used everywhere from servers over games and desktop applications up to security
software and embedded devices. This package provides Haskell bindings to Lua,
enable coders to embed the language into their programs, making them scriptable.

HsLua ships with batteries included and includes Lua 5.4.4. Cabal
flags make it easy to compile against a system-wide Lua
installation.


Interacting with Lua
--------------------

HsLua provides the `Lua` type to define Lua operations. The operations are
executed by calling `run`. A simple "Hello, World" program, using the Lua
`print` function, is given below:

``` haskell
import Foreign.Lua as Lua

main :: IO ()
main = Lua.run prog
  where
    prog :: Lua ()
    prog = do
      Lua.openlibs  -- load Lua libraries so we can use 'print'
      Lua.callFunc "print" "Hello, World!"
```

### The Lua stack

Lua's API is stack-centered: most operations involve pushing values to the stack
or receiving items from the stack. E.g., calling a function is performed by
pushing the function onto the stack, followed by the function arguments in the
order they should be passed to the function. The API function `call` then
invokes the function with given numbers of arguments, pops the function and
parameters off the stack, and pushes the results.

    ,----------.
    |  arg 3   |
    +----------+
    |  arg 2   |
    +----------+
    |  arg 1   |
    +----------+                  ,----------.
    | function |    call 3 1      | result 1 |
    +----------+   ===========>   +----------+
    |          |                  |          |
    |  stack   |                  |  stack   |
    |          |                  |          |

Manually pushing and pulling arguments can become tiresome, so HsLua makes
function calling simple by providing `callFunc`. It uses type-magic to allow
different numbers of arguments. Think about it as having the signature

    callFunc :: String -> a1 -> a2 -> … -> res

where the arguments `a1, a2, …` must be of a type which can be pushed to the Lua
stack, and the result-type `res` must be constructable from a value on the Lua
stack.

### Getting values from and to the Lua stack

Conversion between Haskell and Lua values is governed by two type classes:

``` haskell
-- | A value that can be read from the Lua stack.
class Peekable a where
  -- | Check if at index @n@ there is a convertible Lua value and
  --   if so return it.  Throws a @'LuaException'@ otherwise.
  peek :: StackIndex -> Lua a
```

and

``` haskell
-- | A value that can be pushed to the Lua stack.
class Pushable a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully
  --   nearest Lua type.
  push :: a -> Lua ()
```

Many basic data types (except for numeric types, see the FAQ) have instances for
these type classes. New instances can be defined for custom types using the
functions in `Foreign.Lua.Core` (also exported in `Foreign.Lua`).


Q&A
---

- **Can I see some examples?** Basic examples are available in the
    [*hslua-examples*](https://github.com/hslua/hslua-examples) repository.

    A big project build with hslua is [Pandoc](https://pandoc.org), the
    universal document converter. It is written in Haskell and includes a Lua
    interpreter, enabling programmatic modifications of documents via Lua.
    Furthermore, custom output formats can be defined via Lua scripts.

- **Where are the coroutine related functions?** Yielding from a coroutine works
    via `longjmp`, which plays very badly with Haskell's RTS. Tests to get
    coroutines working with HsLua were unsuccessful. No coroutine related
    functions are exported from the default module for that reason. Pull
    requests intended to fix this are very welcome.
