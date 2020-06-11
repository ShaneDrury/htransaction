{ mkDerivation, base, co-log-core, polysemy, stdenv }:
mkDerivation {
  pname = "co-log-polysemy";
  version = "0.0.1.2";
  sha256 = "96e49cd23fd6d0c33b3d1c11457a6ec51627f16dbb56d589d1d8ae3795c9383c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base co-log-core polysemy ];
  executableHaskellDepends = [ base co-log-core polysemy ];
  homepage = "https://github.com/kowainik/co-log";
  description = "Composable Contravariant Comonadic Logging Library";
  license = stdenv.lib.licenses.mpl20;
}
