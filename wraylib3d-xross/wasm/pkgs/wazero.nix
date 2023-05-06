{ stdenvNoCC }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).wazero);
in
stdenvNoCC.mkDerivation {
  name = "wazero";
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${src} $out/bin/wazero
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/wazero -h
  '';
  allowedReferences = [ ];
}
