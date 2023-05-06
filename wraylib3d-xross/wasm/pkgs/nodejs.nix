{ autoPatchelfHook, stdenv, stdenvNoCC, }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).nodejs);
in
stdenvNoCC.mkDerivation {
  name = "nodejs";
  dontUnpack = true;
  buildInputs = [ stdenv.cc.cc.lib ];
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    cp -a ${src} $out
    chmod -R u+w $out
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/node --version
  '';
  strictDeps = true;
}
