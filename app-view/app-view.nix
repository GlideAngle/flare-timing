{ mkDerivation, aeson, base, containers, ghcjs-base, ghcjs-dom, mtl
, reflex, reflex-dom, scientific, stdenv, text, time
}:
mkDerivation {
  pname = "app-view";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers ghcjs-base ghcjs-dom mtl reflex reflex-dom
    scientific text time
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/blockscope/flare-timing#readme";
  description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
  license = stdenv.lib.licenses.mpl20;
}
