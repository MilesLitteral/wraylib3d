{ stdenvNoCC }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).binaryen);
in
stdenvNoCC.mkDerivation {
  name = "binaryen";
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${src}/* $out/bin
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    MIMALLOC_VERBOSE=1 $out/bin/wasm-opt --version
  '';
  allowedReferences = [ ];
}
