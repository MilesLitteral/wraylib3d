{ autoPatchelfHook, stdenv, stdenvNoCC, }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).wasmtime);
in
stdenvNoCC.mkDerivation {
  name = "wasmtime";
  dontUnpack = true;
  buildInputs = [ stdenv.cc.cc.lib ];
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${src}/wasmtime $out/bin
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/wasmtime --version
  '';
  strictDeps = true;
}
