{ mkDerivation, array, base, containers, deepseq, hspec, microbench
, QuickCheck, stdenv, transformers
}:
mkDerivation {
  pname = "fgl";
  version = "5.5.3.1";
  src = ../.stack2cabal/fgl;
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
