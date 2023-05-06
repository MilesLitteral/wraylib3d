{ autoPatchelfHook, stdenvNoCC, }:
let
  src = builtins.fetchTarball
    ((builtins.fromJSON (builtins.readFile ../autogen.json)).bun);
in
stdenvNoCC.mkDerivation {
  name = "bun";
  dontUnpack = true;
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${src}/bun $out/bin
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    $out/bin/bun --version
  '';
  strictDeps = true;
}
