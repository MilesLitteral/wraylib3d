#!/usr/bin/env -S deno run --allow-read --allow-write --v8-flags=--liftoff-only,--no-wasm-bounds-checks,--no-wasm-stack-checks,--no-wasm-opt,--wasm-lazy-compilation,--wasm-lazy-validation

import WasiContext from "https://deno.land/std/wasi/snapshot_preview1.ts";

function parseArgv(args) {
  const i = args.indexOf("-0");
  return i === -1 ? args : args.slice(i + 2);
}

const argv = parseArgv(Deno.args);

const context = new WasiContext({
  args: argv,
  env: { PATH: "", PWD: Deno.cwd() },
  preopens: { "/": "/" },
});

const instance = (
  await WebAssembly.instantiate(await Deno.readFile(argv[0]), {
    wasi_snapshot_preview1: context.exports,
  })
).instance;

context.start(instance);
