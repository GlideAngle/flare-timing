{ nixpkgs ? <nixpkgs>
, ghcver ? "ghc822"
}:

let

  pkgs = import ./nix/nixpkgs.nix { inherit config; };

  config = {

    packageOverrides = p: {

      # HaskellPackage packages
      haskell = p.haskell // {
        packages = p.haskell.packages // {
          ${ghcver} = p.haskell.packages.${ghcver}.override {
            overrides = self: super: with p.haskell.lib; rec {
              fetchgit = p.fetchgit;
               build-flare-timing = super.callPackage (
                 { mkDerivation, ansi-terminal, base, shake, stdenv, time }:
                 mkDerivation {
                   pname = "build-flare-timing";
                   version = "0.1.0";
                   src = ./build;
                   isLibrary = false;
                   isExecutable = true;
                   executableHaskellDepends = [ ansi-terminal base shake time ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A shake build of flare-timing";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               detour-via-sci = super.callPackage (
                 { mkDerivation, aeson, base, cassava, doctest, newtype, scientific
                 , siggy-chardust, stdenv, template-haskell
                 }:
                 mkDerivation {
                   pname = "detour-via-sci";
                   version = "1.0.0";
                   src = ./detour-via-sci;
                   libraryHaskellDepends = [
                     aeson base cassava newtype scientific siggy-chardust
                     template-haskell
                   ];
                   testHaskellDepends = [
                     aeson base cassava doctest newtype scientific siggy-chardust
                     template-haskell
                   ];
                   homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-sci#readme";
                   description = "JSON and CSV encoding for rationals as decimal point numbers";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               detour-via-uom = dontCheck (super.callPackage (
                 { mkDerivation, aeson, base, cassava, detour-via-sci, doctest
                 , newtype, scientific, stdenv, uom-plugin
                 }:
                 mkDerivation {
                   pname = "detour-via-uom";
                   version = "1.0.0";
                   src = ./detour-via-uom;
                   libraryHaskellDepends = [
                     aeson base cassava detour-via-sci newtype scientific uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base cassava detour-via-sci doctest newtype scientific
                     uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing/tree/master/detour-via-uom#readme";
                   description = "JSON and CSV encoding for quantities";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flare-timing = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, clock, cmdargs, containers
                 , directory, filemanip, filepath, flight-cmd, flight-comp
                 , flight-earth, flight-fsdb, flight-gap, flight-igc, flight-kml
                 , flight-latlng, flight-lookup, flight-mask, flight-route
                 , flight-scribe, flight-span, flight-units, flight-zone, formatting
                 , lens, mtl, raw-strings-qq, siggy-chardust, stdenv, time
                 , transformers, uom-plugin, yaml
                 }:
                 mkDerivation {
                   pname = "flare-timing";
                   version = "0.1.0";
                   src = ./flare-timing;
                   isLibrary = false;
                   isExecutable = true;
                   executableHaskellDepends = [
                     aeson base bytestring clock cmdargs containers directory filemanip
                     filepath flight-cmd flight-comp flight-earth flight-fsdb flight-gap
                     flight-igc flight-kml flight-latlng flight-lookup flight-mask
                     flight-route flight-scribe flight-span flight-units flight-zone
                     formatting lens mtl raw-strings-qq siggy-chardust time transformers
                     uom-plugin yaml
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-cmd = super.callPackage (
                 { mkDerivation, base, cmdargs, directory, filemanip, filepath
                 , flight-span, mtl, raw-strings-qq, stdenv, transformers
                 }:
                 mkDerivation {
                   pname = "flight-cmd";
                   version = "0.1.0";
                   src = ./cmd;
                   libraryHaskellDepends = [
                     base cmdargs directory filemanip filepath flight-span mtl
                     raw-strings-qq transformers
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Command line options";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-comp = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cassava, containers
                 , detour-via-sci, directory, filemanip, filepath, flight-gap
                 , flight-latlng, flight-route, flight-units, flight-zone, lens, mtl
                 , path, scientific, smallcheck, split, stdenv, tasty, tasty-hunit
                 , tasty-quickcheck, tasty-smallcheck, time, unordered-containers
                 , uom-plugin, vector
                 }:
                 mkDerivation {
                   pname = "flight-comp";
                   version = "0.1.0";
                   src = ./comp;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-gap flight-latlng flight-route
                     flight-units flight-zone lens mtl path scientific split time
                     unordered-containers uom-plugin vector
                   ];
                   testHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-gap flight-latlng flight-route
                     flight-units flight-zone lens mtl path scientific smallcheck split
                     tasty tasty-hunit tasty-quickcheck tasty-smallcheck time
                     unordered-containers uom-plugin vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition scoring inputs";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-earth = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, detour-via-sci, fgl
                 , flight-latlng, flight-units, flight-zone, hcoord, hcoord-utm, mtl
                 , numbers, scientific, siggy-chardust, smallcheck, stdenv, tasty
                 , tasty-compare, tasty-hunit, tasty-quickcheck, tasty-smallcheck
                 , uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-earth";
                   version = "0.1.0";
                   src = ./earth;
                   libraryHaskellDepends = [
                     aeson base bifunctors detour-via-sci fgl flight-latlng flight-units
                     flight-zone hcoord hcoord-utm mtl numbers scientific siggy-chardust
                     uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base bifunctors detour-via-sci fgl flight-latlng flight-units
                     flight-zone hcoord hcoord-utm mtl numbers scientific siggy-chardust
                     smallcheck tasty tasty-compare tasty-hunit tasty-quickcheck
                     tasty-smallcheck uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Distances on the WGS84 ellipsoid, the FAI sphere and the UTM projection";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-fsdb = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, containers, detour-via-sci
                 , flight-comp, flight-gap, flight-latlng, flight-units, flight-zone
                 , hxt, hxt-xpath, megaparsec, newtype, path, scientific, smallcheck
                 , split, stdenv, tasty, tasty-hunit, tasty-quickcheck
                 , tasty-smallcheck, time, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-fsdb";
                   version = "0.1.0";
                   src = ./fsdb;
                   libraryHaskellDepends = [
                     aeson base containers detour-via-sci flight-comp flight-gap
                     flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
                     newtype path scientific split time uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base containers detour-via-sci flight-comp flight-gap
                     flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
                     newtype path scientific smallcheck split tasty tasty-hunit
                     tasty-quickcheck tasty-smallcheck time uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A parser for fsdb, the database XML format of FS";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-gap = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, containers, detour-via-sci
                 , detour-via-uom, flight-units, newtype, scientific, siggy-chardust
                 , smallcheck, statistics, stdenv, tasty, tasty-hunit
                 , tasty-quickcheck, tasty-smallcheck, template-haskell, uom-plugin
                 , vector
                 }:
                 mkDerivation {
                   pname = "flight-gap";
                   version = "0.1.0";
                   src = ./gap;
                   libraryHaskellDepends = [
                     aeson base containers detour-via-sci detour-via-uom flight-units
                     newtype scientific siggy-chardust statistics template-haskell
                     uom-plugin vector
                   ];
                   testHaskellDepends = [
                     aeson base containers detour-via-sci detour-via-uom flight-units
                     newtype scientific siggy-chardust smallcheck statistics tasty
                     tasty-hunit tasty-quickcheck tasty-smallcheck template-haskell
                     uom-plugin vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "GAP Scoring";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-igc = super.callPackage (
                 { mkDerivation, base, bytestring, parsec, stdenv, utf8-string }:
                 mkDerivation {
                   pname = "flight-igc";
                   version = "1.0.0";
                   src = ./igc;
                   libraryHaskellDepends = [ base bytestring parsec utf8-string ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A parser for IGC files";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-kml = super.callPackage (
                 { mkDerivation, aeson, base, detour-via-sci, doctest, hxt
                 , hxt-xpath, parsec, path, raw-strings-qq, siggy-chardust
                 , smallcheck, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
                 , tasty-smallcheck, template-haskell, time
                 }:
                 mkDerivation {
                   pname = "flight-kml";
                   version = "1.0.1";
                   src = ./kml;
                   libraryHaskellDepends = [
                     aeson base detour-via-sci hxt hxt-xpath parsec path siggy-chardust
                     split time
                   ];
                   testHaskellDepends = [
                     aeson base detour-via-sci doctest hxt hxt-xpath parsec path
                     raw-strings-qq siggy-chardust smallcheck split tasty tasty-hunit
                     tasty-quickcheck tasty-smallcheck template-haskell time
                   ];
                   homepage = "https://github.com/blockscope/flare-timing/tree/master/kml#readme";
                   description = "Parsing of pilot tracklogs dumped as KML";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-latlng = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, bytestring, cassava
                 , detour-via-sci, detour-via-uom, flight-units, formatting, newtype
                 , numbers, random, siggy-chardust, smallcheck, stdenv
                 , tasty-quickcheck, text, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-latlng";
                   version = "0.1.0";
                   src = ./latlng;
                   libraryHaskellDepends = [
                     aeson base bifunctors bytestring cassava detour-via-sci
                     detour-via-uom flight-units formatting newtype numbers random
                     siggy-chardust smallcheck tasty-quickcheck text uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Latitude and longitude as used in hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-lookup = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cassava, containers
                 , detour-via-sci, directory, filemanip, filepath, flight-comp
                 , flight-gap, flight-kml, flight-latlng, flight-mask, flight-route
                 , flight-zone, lens, mtl, path, scientific, split, stdenv, time
                 , unordered-containers, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-lookup";
                   version = "0.1.0";
                   src = ./lookup;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-comp flight-gap flight-kml flight-latlng
                     flight-mask flight-route flight-zone lens mtl path scientific split
                     time unordered-containers uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition data access";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-mask = dontHaddock (super.callPackage (
                 { mkDerivation, base, bytestring, cmdargs, containers
                 , detour-via-sci, directory, fgl, filepath, flight-comp
                 , flight-earth, flight-gap, flight-kml, flight-latlng, flight-route
                 , flight-scribe, flight-span, flight-task, flight-track
                 , flight-units, flight-zone, lens, mtl, numbers, path
                 , siggy-chardust, split, stdenv, time, uom-plugin, yaml
                 }:
                 mkDerivation {
                   pname = "flight-mask";
                   version = "0.1.0";
                   src = ./mask;
                   libraryHaskellDepends = [
                     base bytestring cmdargs containers detour-via-sci directory fgl
                     filepath flight-comp flight-earth flight-gap flight-kml
                     flight-latlng flight-route flight-scribe flight-span flight-task
                     flight-track flight-units flight-zone lens mtl numbers path
                     siggy-chardust split time uom-plugin yaml
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Track logs masked by competition task zones";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-route = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, detour-via-sci
                 , flight-earth, flight-latlng, flight-task, flight-units
                 , flight-zone, hcoord-utm, numbers, scientific, siggy-chardust
                 , stdenv, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-route";
                   version = "0.1.0";
                   src = ./route;
                   libraryHaskellDepends = [
                     aeson base bifunctors detour-via-sci flight-earth flight-latlng
                     flight-task flight-units flight-zone hcoord-utm numbers scientific
                     siggy-chardust uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Control zones to fly";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-scribe = super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cassava, containers
                 , detour-via-sci, directory, filemanip, filepath, flight-comp
                 , flight-gap, flight-latlng, flight-route, flight-zone, mtl, path
                 , scientific, split, stdenv, time, unordered-containers, vector
                 , yaml
                 }:
                 mkDerivation {
                   pname = "flight-scribe";
                   version = "0.1.0";
                   src = ./scribe;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-comp flight-gap flight-latlng
                     flight-route flight-zone mtl path scientific split time
                     unordered-containers vector yaml
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition scoring files";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-span = super.callPackage (
                 { mkDerivation, base, cmdargs, stdenv }:
                 mkDerivation {
                   pname = "flight-span";
                   version = "0.1.0";
                   src = ./span;
                   libraryHaskellDepends = [ base cmdargs ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "How to measure a distance that spans two points";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-task = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, detour-via-sci, fgl
                 , flight-earth, flight-latlng, flight-units, flight-zone, mtl
                 , numbers, scientific, siggy-chardust, smallcheck, stdenv, tasty
                 , tasty-compare, tasty-hunit, tasty-quickcheck, tasty-smallcheck
                 , uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-task";
                   version = "0.1.0";
                   src = ./task;
                   libraryHaskellDepends = [
                     aeson base bifunctors detour-via-sci fgl flight-earth flight-latlng
                     flight-units flight-zone mtl numbers scientific siggy-chardust
                     uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base bifunctors detour-via-sci fgl flight-earth flight-latlng
                     flight-units flight-zone mtl numbers scientific siggy-chardust
                     smallcheck tasty tasty-compare tasty-hunit tasty-quickcheck
                     tasty-smallcheck uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Tasks to fly";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-track = super.callPackage (
                 { mkDerivation, base, bytestring, containers, directory, filepath
                 , flight-comp, flight-igc, flight-kml, mtl, path, split, stdenv
                 , time, utf8-string
                 }:
                 mkDerivation {
                   pname = "flight-track";
                   version = "0.1.0";
                   src = ./track;
                   libraryHaskellDepends = [
                     base bytestring containers directory filepath flight-comp
                     flight-igc flight-kml mtl path split time utf8-string
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition track logs";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-units = dontHaddock (super.callPackage (
                 { mkDerivation, base, bifunctors, fixed, formatting, numbers
                 , siggy-chardust, stdenv, text, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-units";
                   version = "0.1.0";
                   src = ./units;
                   libraryHaskellDepends = [
                     base bifunctors fixed formatting numbers siggy-chardust text
                     uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Units used in hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-zone = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, aeson-pretty, base, bytestring
                 , detour-via-sci, detour-via-uom, flight-latlng, flight-units, here
                 , newtype, scientific, siggy-chardust, stdenv, tasty
                 , tasty-discover, tasty-golden, tasty-hspec, text, uom-plugin, yaml
                 }:
                 mkDerivation {
                   pname = "flight-zone";
                   version = "0.1.0";
                   src = ./zone;
                   libraryHaskellDepends = [
                     aeson base detour-via-sci detour-via-uom flight-latlng flight-units
                     newtype scientific siggy-chardust uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson aeson-pretty base bytestring detour-via-sci detour-via-uom
                     flight-latlng flight-units here newtype scientific siggy-chardust
                     tasty tasty-discover tasty-golden tasty-hspec text uom-plugin yaml
                   ];
                   testToolDepends = [ tasty-discover ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Control zones to fly";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               hcoord = dontCheck (super.callPackage (
                 { mkDerivation, base, data-default, fetchgit, hlint, HUnit, ieee754
                 , mtl, stdenv
                 }:
                 mkDerivation {
                   pname = "hcoord";
                   version = "3c3859dac5da111e57a6de09764ffdb127197c4a";
                   src = fetchgit {
                     url = "https://github.com/blockscope/hcoord.git";
                     sha256 = "0267n694m08bv73ld7f5flb66h3dxc7xgrbmkr757q0g87l8ndzq";
                     rev = "3c3859dac5da111e57a6de09764ffdb127197c4a";
                     fetchSubmodules = false;
                   };
                   postUnpack = "sourceRoot+=/hcoord; echo source root reset to $sourceRoot";
                   libraryHaskellDepends = [ base mtl ];
                   testHaskellDepends = [ base data-default hlint HUnit ieee754 mtl ];
                   homepage = "https://github.com/danfran/hcoord#readme";
                   description = "Short synopsis";
                   license = stdenv.lib.licenses.bsd3;
                 }
                 ) {});
               hcoord-utm = dontCheck (super.callPackage (
                 { mkDerivation, base, data-default, fetchgit, hcoord, hlint, HUnit
                 , ieee754, mtl, stdenv
                 }:
                 mkDerivation {
                   pname = "hcoord-utm";
                   version = "3c3859dac5da111e57a6de09764ffdb127197c4a";
                   src = fetchgit {
                     url = "https://github.com/blockscope/hcoord.git";
                     sha256 = "0267n694m08bv73ld7f5flb66h3dxc7xgrbmkr757q0g87l8ndzq";
                     rev = "3c3859dac5da111e57a6de09764ffdb127197c4a";
                     fetchSubmodules = false;
                   };
                   postUnpack = "sourceRoot+=/hcoord-utm; echo source root reset to $sourceRoot";
                   libraryHaskellDepends = [ base hcoord mtl ];
                   testHaskellDepends = [
                     base data-default hcoord hlint HUnit ieee754 mtl
                   ];
                   homepage = "https://github.com/danfran/hcoord#readme";
                   description = "Short synopsis";
                   license = stdenv.lib.licenses.bsd3;
                 }
                 ) {});
               siggy-chardust = super.callPackage (
                 { mkDerivation, base, doctest, smallcheck, stdenv, tasty
                 , tasty-hunit, tasty-quickcheck, tasty-smallcheck
                 }:
                 mkDerivation {
                   pname = "siggy-chardust";
                   version = "1.0.0";
                   src = ./siggy-chardust;
                   libraryHaskellDepends = [ base ];
                   testHaskellDepends = [
                     base doctest smallcheck tasty tasty-hunit tasty-quickcheck
                     tasty-smallcheck
                   ];
                   homepage = "https://github.com/blockscope/flare-timing/tree/master/siggy-chardust#readme";
                   description = "Rounding rationals to significant digits and decimal places";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               tasty-compare = super.callPackage (
                 { mkDerivation, base, call-stack, stdenv, tasty, tasty-hunit }:
                 mkDerivation {
                   pname = "tasty-compare";
                   version = "0.1.0";
                   src = ./tasty-compare;
                   libraryHaskellDepends = [ base call-stack tasty tasty-hunit ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Tasty HUnit extensions for comparisons";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               www-flare-timing = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cmdargs, directory
                 , filemanip, filepath, flight-comp, hlint, mtl, raw-strings-qq
                 , servant, servant-server, stdenv, transformers, wai, wai-cors
                 , warp, yaml
                 }:
                 mkDerivation {
                   pname = "www-flare-timing";
                   version = "0.1.0";
                   src = ./www;
                   isLibrary = false;
                   isExecutable = true;
                   executableHaskellDepends = [
                     aeson base bytestring cmdargs directory filemanip filepath
                     flight-comp mtl raw-strings-qq servant servant-server transformers
                     wai wai-cors warp yaml
                   ];
                   testHaskellDepends = [ base flight-comp hlint ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
            };
          };
        };
      };

      # StandardPackage packages
      stack-yaml-packages = p.callPackage ./stack-yaml-packages.nix
                { inherit ghcver;
                  projpkgs=(attrMap (drvForSystem p.system) projpkgs);
                  pkgs=p;
                };
    };
  };



  cleanupSource =
    builtins.filterSource (path: type:
      let nm = baseNameOf path;
        in type != "directory" ||
           !(builtins.elem nm ["dist" "dist-newstyle" ".git" "_darcs"]));

  attrMap = f: s: builtins.listToAttrs
                    (builtins.map
                       (n: { "name"= n; "value"=f n (builtins.getAttr n s); })
                       (builtins.attrNames s));

  drvForSystem = s: n: v: if builtins.hasAttr s v then builtins.getAttr s v else v;

  projpkgs = {
    build-flare-timing = pkgs.haskell.packages.${ghcver}.build-flare-timing;
    detour-via-sci = pkgs.haskell.packages.${ghcver}.detour-via-sci;
    detour-via-uom = pkgs.haskell.packages.${ghcver}.detour-via-uom;
    flare-timing = pkgs.haskell.packages.${ghcver}.flare-timing;
    flight-cmd = pkgs.haskell.packages.${ghcver}.flight-cmd;
    flight-comp = pkgs.haskell.packages.${ghcver}.flight-comp;
    flight-earth = pkgs.haskell.packages.${ghcver}.flight-earth;
    flight-fsdb = pkgs.haskell.packages.${ghcver}.flight-fsdb;
    flight-gap = pkgs.haskell.packages.${ghcver}.flight-gap;
    flight-igc = pkgs.haskell.packages.${ghcver}.flight-igc;
    flight-kml = pkgs.haskell.packages.${ghcver}.flight-kml;
    flight-latlng = pkgs.haskell.packages.${ghcver}.flight-latlng;
    flight-lookup = pkgs.haskell.packages.${ghcver}.flight-lookup;
    flight-mask = pkgs.haskell.packages.${ghcver}.flight-mask;
    flight-route = pkgs.haskell.packages.${ghcver}.flight-route;
    flight-scribe = pkgs.haskell.packages.${ghcver}.flight-scribe;
    flight-span = pkgs.haskell.packages.${ghcver}.flight-span;
    flight-task = pkgs.haskell.packages.${ghcver}.flight-task;
    flight-track = pkgs.haskell.packages.${ghcver}.flight-track;
    flight-units = pkgs.haskell.packages.${ghcver}.flight-units;
    flight-zone = pkgs.haskell.packages.${ghcver}.flight-zone;
    hcoord = pkgs.haskell.packages.${ghcver}.hcoord;
    hcoord-utm = pkgs.haskell.packages.${ghcver}.hcoord-utm;
    siggy-chardust = pkgs.haskell.packages.${ghcver}.siggy-chardust;
    stack-yaml-packages = pkgs.stack-yaml-packages;
    tasty-compare = pkgs.haskell.packages.${ghcver}.tasty-compare;
    www-flare-timing = pkgs.haskell.packages.${ghcver}.www-flare-timing;
  };

in projpkgs

# Usage:
#
#   For any of the primary packages defined above (e.g. build-flare-timing),
#   one of the following can be done:
#
#   $ nix-build -A build-flare-timing flare-timing-project.nix
#     <generates "result" link in local directory>
#
#   $ nix-build -A build-flare-timing.env flare-timing-project.nix
#   nix-shell$ <dev environment for "cabal build">
#
#   $ git clone build-flare-timing
#   $ cd build-flare-timing
#   $ cat > shell.nix << EOF
#   { ghcver ? "ghc822" }:
#   (import /Users/pdejoux/dev/src/blockscope/flare-timing-vernix/flare-timing-project.nix { inherit ghcver; }).build-flare-timing.env
#   EOF
#   $
#          ... and then simply:
#   $ nix-shell
#   nix-shell$ 
#          ... or:
#   $ nix-shell --run "cabal build"
#
#   Note that any package specified will create an environment that
#   references that package and all of its dependencies.
#
