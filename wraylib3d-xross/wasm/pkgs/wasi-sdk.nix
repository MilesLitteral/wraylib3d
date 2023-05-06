{ runtimeShellPackage, stdenvNoCC }:
let
  common-src = builtins.fromJSON (builtins.readFile ../autogen.json);
  wasi-sdk-src = builtins.fetchTarball common-src.wasi-sdk;
  libffi-wasm-src = builtins.fetchTarball common-src.libffi-wasm;
in
stdenvNoCC.mkDerivation {
  name = "wasi-sdk";
  dontUnpack = true;
  installPhase = ''
    cp -a ${wasi-sdk-src} $out
    chmod -R u+w $out

    patchShebangs $out

    cp -a ${libffi-wasm-src}/libffi-wasm/include/. $out/share/wasi-sysroot/include
    cp -a ${libffi-wasm-src}/libffi-wasm/lib/. $out/share/wasi-sysroot/lib/wasm32-wasi
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    pushd "$(mktemp -d)"
    echo '#include <iostream>' >> test.cpp
    echo 'void ffi_alloc_prep_closure(void);' >> test.cpp
    echo 'int main(void) { std::cout << &ffi_alloc_prep_closure << std::endl; }' >> test.cpp
    $out/bin/clang++ test.cpp -lffi -o test.wasm
    popd
  '';
  dontFixup = true;
  allowedReferences = [ runtimeShellPackage ];
}
