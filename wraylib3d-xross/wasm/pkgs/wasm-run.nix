{ callPackage, coreutils, runtimeShell, stdenv, }:
let
  deno = callPackage ./deno.nix { };
  proot = callPackage ./proot.nix { };
  argv = "$" + ''{1+"$@"}'';
in
stdenv.mkDerivation {
  name = "wasm-run";

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out/bin

    cp ${../wasm-run/wasm-run.js} $out/bin/wasm-run.js

    $CC \
      -DWASM_RUN="\"$out/bin/wasm-run.js\"" \
      -Wall \
      -O3 \
      ${../wasm-run/qemu-system-wasm32.c} \
      -o $out/bin/qemu-system-wasm32

    echo '#!${runtimeShell}' >> $out/bin/wasm-run
    echo 'export PATH=${deno}/bin:${coreutils}/bin:$PATH' >> $out/bin/wasm-run
    echo "exec ${proot}/bin/proot -q $out/bin/qemu-system-wasm32" '${argv}' >> $out/bin/wasm-run
    chmod +x $out/bin/wasm-run
  '';

  dontPatchShebangs = true;

  strictDeps = true;
}
