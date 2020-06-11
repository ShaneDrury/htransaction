{ mkDerivation, aeson, base, bytestring, cassava, co-log-polysemy
, containers, filepath, hpack, hspec, hspec-discover, http-client
, http-types, lens, optparse-generic, polysemy, polysemy-plugin
, rainbow, req, stdenv, tagged, text, time
}:
mkDerivation {
  pname = "htransaction";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cassava co-log-polysemy containers filepath
    hspec hspec-discover http-client http-types lens optparse-generic
    polysemy polysemy-plugin rainbow req tagged text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring cassava co-log-polysemy containers filepath
    hspec hspec-discover http-client http-types lens optparse-generic
    polysemy polysemy-plugin rainbow req tagged text time
  ];
  testHaskellDepends = [
    aeson base bytestring cassava co-log-polysemy containers filepath
    hspec hspec-discover http-client http-types lens optparse-generic
    polysemy polysemy-plugin rainbow req tagged text time
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/htransaction#readme";
  license = stdenv.lib.licenses.bsd3;
}
