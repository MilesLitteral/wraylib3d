# `ghc-wasm-meta`

This repo provides convenient methods of using x86_64-linux binary
artifacts of GHC's wasm backend. It's also a staging area for the GHC
wasm backend documentation.

## Getting started as a nix flake

This repo is a nix flake. The default output is a derivation that
bundles all provided tools:

```sh
$ nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz
$ echo 'main = putStrLn "hello world"' > hello.hs
$ wasm32-wasi-ghc hello.hs -o hello.wasm
[1 of 2] Compiling Main             ( hello.hs, hello.o )
[2 of 2] Linking hello.wasm
$ wasmtime ./hello.wasm
hello world
```

First start will download a bunch of stuff, but won't take too long
since it just patches the binaries and performs no compilation. There
is no need to set up a binary cache.

Note that there are different GHC flavours available. The default
flake output uses the `gmp` flavour, though you can also use the
`all_native`, `all_unreg` and `all_9_6` flake outputs to get different
flavours. See the next subsection for explanations of these flavours.

## Getting started without nix

This repo also provides a `setup.sh` script that installs the provided
tools to `~/.ghc-wasm`:

```sh
$ ./setup.sh
...
Everything set up in /home/username/.ghc-wasm.
Run 'source /home/username/.ghc-wasm/env' to add tools to your PATH.
```

After installing, `~/.ghc-wasm` will contain all the installed tools,
plus:

  - `~/.ghc-wasm/env` which can be sourced into the current shell to
    add the tools to `PATH`, plus all the environment variables needed
    to build `wasm32-wasi-ghc`
  - `~/.ghc-wasm/add_to_github_path.sh` which can be run in GitHub
    actions, so later steps in the same job can access the same
    environment variables set by `env`

`setup.sh` can be configured via these environment variables:

  - `PREFIX`: installation destination, defaults to `~/.ghc-wasm`
  - `FLAVOUR`: can be `gmp`, `native` or `unreg`.
    - The `gmp` flavour uses the `gmp` bignum backend and the wasm
      native codegen. It's the default flavour, offers good
      compile-time and run-time performance.
    - `native` uses the `native` bignum backend and the wasm native
      codegen. Compared to the `gmp` flavour, the run-time performance
      may be slightly worse if the workload involves big `Integer`
      operations. May be useful if you are compiling proprietary
      projects and have concerns about statically linking the
      LGPL-licensed `gmp` library.
    - The `unreg` flavour uses the `gmp` bignum backend and the
      unregisterised C codegen. Compared to the default flavour,
      compile-time performance is noticeably worse. May be useful for
      debugging the native codegen, since there are less GHC test
      suite failures in the unregisterised codegen at the moment.
    - The `9.6` flavour tracks the `ghc-9.6` release branch instead of
      the `master` branch. It uses the `gmp` bignum backend and the
      wasm native codegen.
  - `SKIP_GHC`: set this to skip installing `cabal` and `ghc`

Note that if you use the `9.6` flavour, the `wasm32-wasi-cabal`
wrapper won't automatically set up `head.hackage` in the global config
file. In the early days of `ghc-9.6`, this may result in more packages
being rejected at compile time. This is true for both nix/non-nix
installation methods.

`setup.sh` requires `cc`, `curl`, `jq`, `unzip` to run.

## What it emits when it emits a `.wasm` file?

Besides wasm MVP, certain extensions are used. The feature flags are
enabled globally in our
[wasi-sdk](https://gitlab.haskell.org/ghc/wasi-sdk) build, passed at
GHC configure time, and the wasm NCG may make use of the features. The
rationale of post-MVP wasm feature inclusion:

- Supported by default in latest versions of
Chrome/Firefox/Safari/wasmtime (check wasm
[roadmap](https://webassembly.org/roadmap) for details)
- LLVM support has been stable enough (doesn't get into our way when
enabled globally)

List of wasm extensions that we use:

- [128-bit packed
  SIMD](https://github.com/WebAssembly/spec/blob/master/proposals/simd/SIMD.md)
- [Non-trapping Float-to-int
  Conversions](https://github.com/WebAssembly/spec/blob/master/proposals/nontrapping-float-to-int-conversion/Overview.md)
- [Sign-extension
  operators](https://github.com/WebAssembly/spec/blob/master/proposals/sign-extension-ops/Overview.md)
- [Bulk Memory
  Operations](https://github.com/WebAssembly/spec/blob/master/proposals/bulk-memory-operations/Overview.md)
- [Import/Export mutable
  globals](https://github.com/WebAssembly/mutable-global/blob/master/proposals/mutable-global/Overview.md)
- [Multi-value](https://github.com/WebAssembly/spec/blob/master/proposals/multi-value/Overview.md)
- [Reference
  Types](https://github.com/WebAssembly/spec/blob/master/proposals/reference-types/Overview.md)

The target triple is `wasm32-wasi`, and it uses WASI snapshot 1 as
used in `wasi-libc`.

List of wasm extensions that we don't use yet but are keeping an eye
on:

- [Tail
  Call](https://github.com/WebAssembly/tail-call/blob/main/proposals/tail-call/Overview.md),
  blocked by
  [wasmtime](https://github.com/bytecodealliance/wasmtime/issues/1065)
  and a few other engines. Note that we already support wasm tail
  calls, but it's an opt-in feature for now.

## What runtimes support those `.wasm` files?

The output `.wasm` modules are known to run on these runtimes:

### Non-browser non-JavaScript runtimes

These tools are all packaged in this repo, available in both the nix
flake & `setup.sh` installation. The recommended default runtime is
`wasmtime`, other ones are included for testing purposes.

- [`wasmtime`](https://wasmtime.dev)
- [`wasmedge`](https://wasmedge.org)
- [`wazero`](https://wazero.io)

### Non-browser JavaScript runtimes

- [`deno`](https://deno.land), using
  https://deno.land/std/wasi/snapshot_preview1.ts as WASI
  implementation
- [`node`](https://nodejs.org), using the builtin
  [`wasi`](https://nodejs.org/api/wasi.html) module to provide WASI
  implementation
- [`bun`](https://bun.sh), using the builtin WASI
  [implementation](https://github.com/oven-sh/bun/blob/main/src/bun.js/wasi.exports.js)

### Browsers

Latest releases of Chrome/Firefox/Safari. A JavaScript library is
needed to provide the WASI implementation, the following are known to
work:

- [`wasi-js`](https://github.com/sagemathinc/cowasm/tree/main/core/wasi-js)
- [`browser_wasi_shim`](https://github.com/bjorn3/browser_wasi_shim)

## Compiling to WASI reactor module with user-specified exports

If you want to embed the compiled wasm module into a host language,
like in JavaScript for running in a browser, then it's highly likely
you want to compile Haskell to a WASI reactor module.

### What is a WASI reactor module?

The WASI spec includes certain
[syscalls](https://github.com/WebAssembly/WASI/blob/main/legacy/preview1/docs.md)
that are provided as the `wasi_snapshot_preview1` wasm imports.
Additionally, the current WASI
[ABI](https://github.com/WebAssembly/WASI/blob/main/legacy/application-abi.md)
specifies two different kinds of WASI modules: commands and reactors.

A WASI command module is what you get by default when using
`wasm32-wasi-ghc` to compile & link a Haskell program. It's called
"command" as in a conventional command-line program, with similar
usage and lifecycle: run it with something like `wasmtime`, optionally
passing some arguments and environment variables, it'll run to
completion and probably causing some side effects using whatever
capabilities granted. After it runs to completion, the program state
is finalized.

A WASI reactor module is produced by special link-time flags. It's
called "reactor" since it reacts to external calls of its exported
functions. Once a reactor module is initialized, the program state is
persisted, so if calling an export changes internal state (e.g. sets a
global variable), subsequent calls will observe that change.

### Why the distinction and why should you care?

When linking a program for almost any platform out there, the linker
needs to handle ctors(constructors) & dtors(destructors). ctors and
dtors are special functions that need to be invoked to correctly
initialize/finalize certain runtime state. Even if the user program
doesn't use ctors/dtors, as long as the program links to libc,
ctors/dtors will need to be handled.

The wasm spec does include a [start
function](https://webassembly.github.io/spec/core/syntax/modules.html#syntax-start).
However, due to technical
[reasons](https://github.com/WebAssembly/design/issues/1160), what a
start function can do is rather limited, and may not be sufficient to
support ctors/dtors in libc and other places. So the WASI spec needs
to address this fact, and the command/reactor distinction arises:

- A WASI command module must export a `_start` function. You can see
  how `_start` is defined in `wasi-libc`
  [here](https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/libc-bottom-half/crt/crt1-command.c).
  It'll call the ctors, then call the main function in user code, and
  finally call the dtors. Since the dtors are called, the program
  state is finalized, so attempting to call any export after this
  point is undefined behavior!
- A WASI reactor module may export an `_initialize` function, if it
  exists, it must be called exactly once before any other exports are
  called. See its definition
  [here](https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/libc-bottom-half/crt/crt1-reactor.c),
  it merely calls the ctors. So after `_initialize`, you can call the
  exports freely, reusing the instance state. If you want to
  "finalize", you're in charge of exporting and calling
  `__wasm_call_dtors` yourself.

The command module works well for wasm modules that are intended to be
used like a conventional CLI app. On the otherhand, for more advanced
use cases like running in a browser, you almost always want to create
a reactor module instead.

### Creating a WASI reactor module from `wasm32-wasi-ghc`

Suppose there's a `Hello.hs` that has a `fib :: Int -> Int`. To invoke
it from the JavaScript host, first you need to write down a `foreign
export` for it:

```haskell
foreign export ccall fib :: Int -> Int
```

GHC will create a C function `HsInt fib(HsInt)` that calls into the
actual `fib` Haskell function. Now you need to compile and link it
with special flags:

```sh
$ wasm32-wasi-ghc Hello.hs -o Hello.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init,--export=myMain
```

Some explainers:

- `-no-hs-main`, since we only care about manually exported functions
  and don't have a default `main :: IO ()`
- `-optl-mexec-model=reactor` passes `-mexec-model=reactor` to `clang`
  when linking, so it creates a WASI reactor instead of a WASI command
- `-optl-Wl,--export=hs_init,--export=fib` passes the linker flags to
  export `hs_init` and `fib`
- `-o Hello.wasm` is necessary, otherwise the output name defaults to
  `a.out` which can be confusing

The flags above also work in the `ghc-options` field of a cabal
executable component, see
[here](https://github.com/tweag/ormolu/blob/master/ormolu-live/ormolu-live.cabal)
for an example.

Now, here's an example `deno` script to load and run `Hello.wasm`:

```javascript
import WasiContext from "https://deno.land/std/wasi/snapshot_preview1.ts";

const context = new WasiContext({});

const instance = (
  await WebAssembly.instantiate(await Deno.readFile("Hello.wasm"), {
    wasi_snapshot_preview1: context.exports,
  })
).instance;

// The initialize() method will call the module's _initialize export
// under the hood. This is only true for the wasi implementation used
// in this example! If you're using another wasi implementation, do
// read its source code to figure out whether you need to manually
// call the module's _initialize export!
context.initialize(instance);

// This function is a part of GHC's RTS API. It must be called before
// any other exported Haskell functions are called.
instance.exports.hs_init(0, 0);

console.log(instance.exports.fib(10));
```

For simplicity, we call `hs_init` with `argc` set to `0` and `argv`
set to `NULL`, assuming we don't use things like
`getArgs`/`getProgName` in the program. Now, we can call `fib`, or any
function with `foreign export` and the correct `--export=` flag.

Before we add first-class JavaScript interop feature, it's only
possible to use the `ccall` calling convention for foreign exports.
It's still possible to exchange large values between Haskell and
JavaScript:

- Add `--export` flag for `malloc`/`free`. You can now allocate and
  free linear memory buffers that can be visible to the Haskell world,
  since the entire linear memory is available as the `memory` export.
- In the Haskell world, you can pass `Ptr` as foreign export
  argument/return values.
- You can also use `mallocBytes` in `Foreign.Marshal.Alloc` to
  allocate buffers in the Haskell world. A buffer allocated by
  `mallocBytes` in Haskell can be passed to JavaScript and be freed by
  the exported `free`, and vice versa.

If you pass a buffer `malloc`ed in JavaScript into Haskell, before
`free`ing that buffer after the exported Haskell function returns,
look into the Haskell code and double check if that buffer is indeed
fully consumed in Haskell! Otherwise, use-after-free bugs await.

Which functions can be exported via the `--export` flag?

- Any C function which symbol is externally visible. For libc, there
  is a
  [list](https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/expected/wasm32-wasi/defined-symbols.txt)
  of all externally visible symbols. For the GHC RTS, see
  [`HsFFI.h`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/include/HsFFI.h)
  and
  [`RtsAPI.h`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/include/RtsAPI.h)
  for the functions that you're likely interested in. For instance,
  `hs_init` is in `HsFFI.h`, other variants of `hs_init*` is in
  `RtsAPI.h`.
- Any Haskell function that has been exported via a `foreign export`
  declaration.

TODO:

- Table of Haskell/JavaScript type marshaling
- Example of setting RTS options
- Example of working with dynamic exports (`foreign import ccall
  "wrapper"`)
- Example of handling exceptions

Further reading:

- [Using the FFI with
  GHC](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#using-the-ffi-with-ghc)
- [WebAssembly lld port](https://lld.llvm.org/WebAssembly.html)

### Custom imports

In addition to the standard `wasi_snapshot_preview1` import module,
you can add other import modules to import host JavaScript functions
which can be called in C/Haskell. Of course, the resulting wasm module
is not completely compliant to the WASI spec, and cannot be run in
non-browser runtimes that only supports WASI (like `wasmtime`), but
it's not an issue if it's only intended to be run in browsers.

Here's an example:

```c
#include <stdint.h>

void _fill(void *buf, uint32_t len) __attribute__((
  __import_module__("env"),
  __import_name__("foo")
));

void fill(void *buf, uint32_t len) {
  _fill(buf, len);
}
```

The above assembly source file can be saved as `fill.c`, and
compiled/linked with other C/Haskell sources. The `fill` function can
be called in C as long as you write down its prototype; it can also be
called in Haskell just like any other normal C function.

Now, when creating the instance, in addition to providing the
`wasi_snapshot_preview1` import module, you also need to provide the
`env` import module which has a `foo` function. Whatever `foo` can do
is up to you, reader. It may take a buffer's pointer/length and fill
in something interesting. Since the WASI module has a `memory` export,
`foo` has access to the instance memory and can do whatever it want.

Custom imports is a powerful feature and provides more than one way to
do things. For instance, if your Haskell program needs to read a large
blob, you may either put that blob in a memfs and read it as if
reading a file; or you can just feed that blob via a custom import. In
this case, a custom import may be simpler, and it further lowers the
requirement on the WASI implementation, since not even a virtual memfs
implementation will be needed at run-time.

It's even possible for the imported function to invoke Haskell
computation by calling exported Haskell functions! This kind of RTS
re-entrance is permitted in GHC RTS, as long as the custom import is
marked with `foreign import ccall safe`.

### Using `wizer` to pre-initialize a WASI reactor module

[`wizer`](https://github.com/bytecodealliance/wizer) is a tool that
takes a wasm module, runs a user-specified initialization function,
then snapshots the wasm instance state into a new wasm module. Since
`wizer` is based on `wasmtime`, it supports WASI modules out of the
box.

I recommend using `wizer` to pre-initialize your WASI reactor module
compiled from Haskell. It's not just about avoiding the overhead of
`_initialize`; the initialization function run by `wizer` is capable
of much more tasks, including but not limited to:

- Set up custom RTS flags and other command line arguments
- Perform arbitrary Haskell computation
- Perform Haskell garbage collection to re-arrange the heap in an
  optimal way

It requires a bit of knowledge about GHC's RTS API to write this
initialization function, here's an example:

```c
// Including this since we need access to GHC's RTS API. And it
// transitively includes pretty much all of libc headers that we need.
#include <Rts.h>

// When GHC compiles the Test module with foreign export, it'll
// generate Test_stub.h that declares the prototypes for C functions
// that wrap the corresponding Haskell functions.
#include "Test_stub.h"

// The prototype of hs_init_with_rtsopts is "void
// hs_init_with_rtsopts(int *argc, char **argv[])" which is a bit
// cumbersome to work with, hence this convenience wrapper.
STATIC_INLINE void hs_init_with_rtsopts_(char *argv[]) {
  int argc;
  for (argc = 0; argv[argc] != NULL; ++argc) {
  }
  hs_init_with_rtsopts(&argc, &argv);
}

// Export this function as "wizer.initialize". wizer also accepts
// "--init-func <init-func>" if you dislike this export name, or
// prefer to pass -Wl,--export=my_init at link-time.
//
// By the time this function is called, the WASI reactor _initialize
// has already been called by wizer. The export entries of this
// function and _initialize will both be stripped by wizer.
__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  // The first argument is what you get in getProgName.
  //
  // --nonmoving-gc is recommended when compiling to WASI reactors,
  // since you're likely more concerned about GC pause time than the
  // overall throughput.
  //
  // -H64m sets the "suggested heap size" to 64MB and reserves so much
  // memory when doing GC for the first time. It's not a hard limit,
  // the RTS is perfectly capable of growing the heap beyond it, but
  // it's still recommended to reserve a reasonably sized heap in the
  // beginning. And it doesn't add 64MB to the wizer output, most of
  // the grown memory will be zero anyway!
  char *argv[] = {"test.wasm", "+RTS", "--nonmoving-gc", "-H64m", "-RTS", NULL};

  // The WASI reactor _initialize function only takes care of
  // initializing the libc state. The GHC RTS needs to be initialized
  // using one of hs_init* functions before doing any Haskell
  // computation.
  hs_init_with_rtsopts_(argv);

  // Not interesting, I know. The point is you can perform any Haskell
  // computation here! Or C/C++, whatever.
  fib(10);

  // Perform a major GC to clean up the heap. When using the nonmoving
  // garbage collector, it's necessary to call it twice to actually
  // free the unused segments.
  hs_perform_gc();
  hs_perform_gc();

  // Finally, zero out the unused RTS memory, to prevent the garbage
  // bytes from being snapshotted into the final wasm module.
  // Otherwise it wouldn't affect correctness, but the wasm module
  // size would bloat significantly. It's only safe to call this after
  // hs_perform_gc() has returned.
  rts_clearMemory();
}
```

Then you can compile & link the C code above with a regular Haskell
module, and pre-initialize using `wizer`:

```sh
wasm32-wasi-ghc test.hs test_c.c -o test.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=fib
wizer --allow-wasi --wasm-bulk-memory true test.wasm -o test.wizer.wasm
```

Note that `test.wizer.wasm` will be slightly larger than `test.wasm`,
which is expected behavior here, given some computation has already
been run and the linear memory captures more runtime data.

If you run `wasm-opt` to minimize the `wasm` module, it's recommend to
only run it for the `wizer` output. `wasm-opt` will be able to strip
away some unused initialization functions that are no longer reachable
via wasm exports or function table.

## Accessing the host file system in non-browsers

By default, only stdin/stdout/stderr is supported. To access the host
file system, one needs to map the allowed root directory into `/` as a
WASI preopen.

The initial working directoy is always `/`. If you'd like to specify
another working directory other than `/` in the virtual filesystem,
use the `PWD` environment variable. This is not a WASI standard, just
a workaround we implemented in the GHC RTS shall the need arises.

## Building guide

If you intend to build the GHC's wasm backend, here's a building
guide, assuming you already have some experience with building native
GHC.

### Install `wasi-sdk`

To build the wasm backend, the systemwide C/C++ toolchain won't work.
You need to install our `wasi-sdk`
[fork](https://gitlab.haskell.org/ghc/wasi-sdk). Upstream `wasi-sdk`
won't work yet.

If your host system is one of `{x86_64,aarch64}-{linux,darwin}`, then
you can avoid building and simply download & extract the GitLab CI
artifact from the latest `master` commit. The linux artifacts are
statically linked and work out of the box on all distros; the macos
artifact contains universal binaries and works on either apple silicon
or intel mac.

If your host system is `x86_64-linux`, you can use the `setup.sh`
script to install it, as documented in previous sections.

For simplicity, the following subsections all assume `wasi-sdk` is
installed to `~/.ghc-wasm/wasi-sdk`, and
`~/.ghc-wasm/wasi-sdk/bin/clang` works.

### Install `libffi-wasm`

Skip this subsection if `wasi-sdk` is installed by `setup.sh` instead
of extracting CI artifacts directly.

Extract the CI artifact of
[`libffi-wasm`](https://gitlab.haskell.org/ghc/libffi-wasm), and copy
its contents:

- `cp *.h ~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/include`
- `cp *.a ~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi`

### Set environment variables

If you used `setup.sh` to install `wasi-sdk`/`libffi-wasm`, then you
can `source ~/.ghc-wasm/env` into your current shell to set the
following environment variables required for building. Otherwise, you
can save this to somewhere else and source that script.

```bash
export AR=~/.ghc-wasm/wasi-sdk/bin/llvm-ar
export CC=~/.ghc-wasm/wasi-sdk/bin/clang
export CC_FOR_BUILD=cc
export CXX=~/.ghc-wasm/wasi-sdk/bin/clang++
export LD=~/.ghc-wasm/wasi-sdk/bin/wasm-ld
export NM=~/.ghc-wasm/wasi-sdk/bin/llvm-nm
export OBJCOPY=~/.ghc-wasm/wasi-sdk/bin/llvm-objcopy
export OBJDUMP=~/.ghc-wasm/wasi-sdk/bin/llvm-objdump
export RANLIB=~/.ghc-wasm/wasi-sdk/bin/llvm-ranlib
export SIZE=~/.ghc-wasm/wasi-sdk/bin/llvm-size
export STRINGS=~/.ghc-wasm/wasi-sdk/bin/llvm-strings
export STRIP=~/.ghc-wasm/wasi-sdk/bin/llvm-strip
export CONF_CC_OPTS_STAGE2="-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"
export CONF_CXX_OPTS_STAGE2="-Wno-error=int-conversion -Wno-error=strict-prototypes -Wno-error=implicit-function-declaration -fno-exceptions -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types"
export CONF_GCC_LINKER_OPTS_STAGE2="-Wl,--compress-relocations,--error-limit=0,--growable-table,--stack-first,--strip-debug -Wno-unused-command-line-argument"
export CONFIGURE_ARGS="--target=wasm32-wasi --with-intree-gmp --with-system-libffi"
```

### Checkout & patch GHC

Checkout GHC. Latest `master` revision contains wasm backend support.
Do apply the patches
[here](https://gitlab.haskell.org/ghc/ghc/-/blob/cc25d52e0f65d54c052908c7d91d5946342ab88a/.gitlab-ci.yml#L816),
they may get removed some time later but for now they're mandatory to
get the wasm backend building.

### Boot & configure & build GHC

The rest is the usual boot & configure & build process. You need to
ensure the environment variables described earlier are correctly set
up; for `ghc.nix` users, it sets up a default `CONFIGURE_ARGS` in the
nix-shell which is incompatible, and the `env` script set up by
`setup.sh` respects existing `CONFIGURE_ARGS`, so don't forget to
unset it first!

Configure with `./configure $CONFIGURE_ARGS`, then build with hadrian.
After the build completes, you can compile stuff to wasm using
`_build/stage1/bin/wasm32-wasi-ghc`.

Happy hacking!

## When something goes wrong

### Reporting issues

If you suspect there's something wrong that needs fixing in the wasm
backend (e.g. runtime crashes), you're more than welcome to open a
ticket in the GHC issue tracker! The `wasm` tag will be added to the
ticket by one of the triagers, which ensures it ends up in my inbox.

It would be very nice to have a self-contained Haskell source file to
reproduce the bug, but it's not a strict necessity to report the bug.
It's fine as long as the project source code and build instruction is
available.

Do include the following info in the ticket:

- Which `ghc-wasm-meta` revision is used to install `wasm32-wasi-ghc`?
  Or if you're building it yourself, which GHC revision are you using?

### Getting more insight on the bug

To get some more insight on the bug, there are a few things worth
checking, importance ranked from high to low:

- Try a different runtime environment. If it is a WASI command module
  (self-contained `.wasm` file), does it work in `wasmtime` or some
  other runtime? If it is meant for the browser, try switching to a
  different WASI implementation, does the same bug still occur? It's
  known that all current JavaScript implementations of WASI are not
  100% feature complete and may have bugs.
- Try a different optimization level in GHC. If it's a cabal project,
  you can configure the
  [`optimization:`](https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-optimization)
  field in `cabal.project`. In case of a code generation bug, it may
  be the case that one of the passes during GHC optimization resulted
  in buggy code.
- Try enabling `-dlint` at compile-time to check consistency.
- If it crashes, make it crash as early/verbose as possible. Use the
  GHC `-debug` flag at link time to link with the debug RTS that
  enables certain internal checks. On top of that, pass RTS flags to
  enable certain checks, e.g. `+RTS -DS` for sanity checks, see
  [here](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/runtime_control.html#rts-options-for-hackers-debuggers-and-over-interested-souls)
  for details.
