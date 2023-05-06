#!/usr/bin/env -S node --no-warnings --experimental-wasi-unstable-preview1 --liftoff-only --no-wasm-bounds-checks --no-wasm-stack-checks --no-wasm-opt --wasm-lazy-compilation --wasm-lazy-validation

import fs from "node:fs/promises";
import { WASI } from "node:wasi";

function parseArgv(args) {
  const i = args.indexOf("-0");
  return i === -1 ? args : args.slice(i + 2);
}

const argv = parseArgv(process.argv.slice(2));

const wasi = new WASI({
  version: "preview1",
  args: argv,
  env: { PATH: "", PWD: process.cwd() },
  preopens: { "/": "/" },
});

const instance = (
  await WebAssembly.instantiate(await fs.readFile(argv[0]), {
    wasi_snapshot_preview1: wasi.wasiImport,
  })
).instance;

wasi.start(instance);
