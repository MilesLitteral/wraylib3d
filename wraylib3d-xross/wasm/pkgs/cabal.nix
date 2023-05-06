{ stdenvNoCC }:
let
  src = builtins.fetchurl
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).cabal);
in
stdenvNoCC.mkDerivation {
  name = "cabal";
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/bin
    tar xJf ${src} -C $out/bin 'cabal'
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/cabal --version
  '';
  allowedReferences = [ ];
}
