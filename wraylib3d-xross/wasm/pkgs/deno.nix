{ autoPatchelfHook, stdenv, stdenvNoCC, }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).deno);
in
stdenvNoCC.mkDerivation {
  name = "deno";
  dontUnpack = true;
  buildInputs = [ stdenv.cc.cc.lib ];
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${src} $out/bin/deno
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/deno --version
  '';
  strictDeps = true;
}
