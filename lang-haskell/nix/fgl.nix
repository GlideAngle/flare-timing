{ mkDerivation, array, base, containers, deepseq, fetchgit, hspec
, microbench, QuickCheck, stdenv, transformers
}:
mkDerivation {
  pname = "fgl";
  version = "5.5.3.1";
  src = fetchgit {
    url = "http://github.com/haskell/fgl.git";
    sha256 = "0biwsaj6s24l8ix95hkw4syl87ywxy363pr413kazzbhi0csf20s";
    rev = "e29503775e474b7a7cd8951b6bebcf1529b888b5";
  };
  libraryHaskellDepends = [
    array base containers deepseq transformers
  ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  benchmarkHaskellDepends = [ base deepseq microbench ];
  doHaddock = false;
  doCheck = false;
  description = "Martin Erwig's Functional Graph Library";
  license = stdenv.lib.licenses.bsd3;
}
