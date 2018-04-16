{ mkDerivation, base, lens, stdenv }:
mkDerivation {
  pname = "demo-game";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
