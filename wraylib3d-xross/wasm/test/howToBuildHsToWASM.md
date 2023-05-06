wasm32-wasi-ghc hello.hs -o hello.wasm
-- [1 of 2] Compiling Main             ( hello.hs, hello.o )
-- [2 of 2] Linking hello.wasm
wasmtime ./hello.wasm
hello world