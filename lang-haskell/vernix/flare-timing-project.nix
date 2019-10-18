{ nixpkgs ? <nixpkgs>
, ghcver ? "ghc822"
}:

let

  pkgs = import nixpkgs { inherit config; };

  config = {

    packageOverrides = p: {

      # HaskellPackage packages
      haskell = p.haskell // {
        packages = p.haskell.packages // {
          ${ghcver} = p.haskell.packages.${ghcver}.override {
            overrides = self: super: with p.haskell.lib; rec {
              fetchgit = p.fetchgit;
               app-serve = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cmdargs, directory
                 , filemanip, filepath, flight-clip, flight-cmd, flight-comp
                 , flight-gap, flight-kml, flight-latlng, flight-mask, flight-route
                 , flight-scribe, hlint, mtl, raw-strings-qq, safe-exceptions
                 , servant, servant-server, siggy-chardust, stdenv, time
                 , transformers, uom-plugin, wai, wai-cors, wai-extra, warp, yaml
                 }:
                 mkDerivation {
                   pname = "app-serve";
                   version = "0.1.0";
                   src = ../app-serve;
                   isLibrary = false;
                   isExecutable = true;
                   executableHaskellDepends = [
                     aeson base bytestring cmdargs directory filemanip filepath
                     flight-clip flight-cmd flight-comp flight-gap flight-kml
                     flight-latlng flight-mask flight-route flight-scribe mtl
                     raw-strings-qq safe-exceptions servant servant-server
                     siggy-chardust time transformers uom-plugin wai wai-cors wai-extra
                     warp yaml
                   ];
                   testHaskellDepends = [ base flight-comp hlint ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               detour-via-sci = super.callPackage (
                 { mkDerivation, aeson, base, cassava, doctest, newtype, scientific
                 , siggy-chardust, stdenv, template-haskell
                 }:
                 mkDerivation {
                   pname = "detour-via-sci";
                   version = "1.0.1";
                   src = ../detour-via-sci;
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
                   version = "1.0.1";
                   src = ../detour-via-uom;
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
               doctest = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, base, base-compat, code-page, deepseq, directory
                 , filepath, ghc, ghc-paths, hspec, HUnit, mockery, process
                 , QuickCheck, setenv, silently, stdenv, stringbuilder, syb
                 , transformers, with-location
                 }:
                 mkDerivation {
                   pname = "doctest";
                   version = "0.15.0";
                   sha256 = "f1ae62f740fbf287e067283cebdc3cd9eef447e8e52865efebeb67c418a2818d";
                   isLibrary = true;
                   isExecutable = true;
                   libraryHaskellDepends = [
                     base base-compat code-page deepseq directory filepath ghc ghc-paths
                     process syb transformers
                   ];
                   executableHaskellDepends = [
                     base base-compat code-page deepseq directory filepath ghc ghc-paths
                     process syb transformers
                   ];
                   testHaskellDepends = [
                     base base-compat code-page deepseq directory filepath ghc ghc-paths
                     hspec HUnit mockery process QuickCheck setenv silently
                     stringbuilder syb transformers with-location
                   ];
                   homepage = "https://github.com/sol/doctest#readme";
                   description = "Test interactive Haskell examples";
                   license = stdenv.lib.licenses.mit;
                 }
                 ) {}));
               flare-timing = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, clock, cmdargs, containers
                 , directory, filemanip, filepath, flight-clip, flight-cmd
                 , flight-comp, flight-earth, flight-fsdb, flight-gap, flight-igc
                 , flight-kml, flight-latlng, flight-lookup, flight-mask
                 , flight-route, flight-scribe, flight-span, flight-time
                 , flight-units, flight-zone, formatting, lens, mtl, raw-strings-qq
                 , safe-exceptions, siggy-chardust, statistics, stdenv, time
                 , transformers, uom-plugin, vector, yaml
                 }:
                 mkDerivation {
                   pname = "flare-timing";
                   version = "0.1.0";
                   src = ../flare-timing;
                   isLibrary = false;
                   isExecutable = true;
                   executableHaskellDepends = [
                     aeson base bytestring clock cmdargs containers directory filemanip
                     filepath flight-clip flight-cmd flight-comp flight-earth
                     flight-fsdb flight-gap flight-igc flight-kml flight-latlng
                     flight-lookup flight-mask flight-route flight-scribe flight-span
                     flight-time flight-units flight-zone formatting lens mtl
                     raw-strings-qq safe-exceptions siggy-chardust statistics time
                     transformers uom-plugin vector yaml
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A collection of apps and libraries for scoring hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-clip = dontCheck (super.callPackage (
                 { mkDerivation, base, doctest, split, stdenv, time }:
                 mkDerivation {
                   pname = "flight-clip";
                   version = "1.1.0";
                   src = ../clip;
                   libraryHaskellDepends = [ base split time ];
                   testHaskellDepends = [ base doctest split time ];
                   homepage = "https://github.com/blockscope/flare-timing/tree/master/clip#readme";
                   description = "Clipping a pilot's tracklogs";
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
                   src = ../cmd;
                   libraryHaskellDepends = [
                     base cmdargs directory filemanip filepath flight-span mtl
                     raw-strings-qq transformers
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Command line options";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-comp = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bytestring, cassava, containers
                 , detour-via-sci, detour-via-uom, directory, doctest, filemanip
                 , filepath, flight-clip, flight-earth, flight-gap, flight-latlng
                 , flight-route, flight-units, flight-zone, lens, mtl, newtype, path
                 , scientific, siggy-chardust, smallcheck, split, stdenv, tasty
                 , tasty-hunit, tasty-quickcheck, tasty-smallcheck, text, time
                 , unordered-containers, uom-plugin, vector
                 }:
                 mkDerivation {
                   pname = "flight-comp";
                   version = "0.1.0";
                   src = ../comp;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci
                     detour-via-uom directory filemanip filepath flight-clip
                     flight-earth flight-gap flight-latlng flight-route flight-units
                     flight-zone lens mtl newtype path scientific siggy-chardust split
                     text time unordered-containers uom-plugin vector
                   ];
                   testHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci
                     detour-via-uom directory doctest filemanip filepath flight-clip
                     flight-earth flight-gap flight-latlng flight-route flight-units
                     flight-zone lens mtl newtype path scientific siggy-chardust
                     smallcheck split tasty tasty-hunit tasty-quickcheck
                     tasty-smallcheck text time unordered-containers uom-plugin vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition scoring inputs";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-earth = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, detour-via-sci
                 , detour-via-uom, doctest, fgl, flight-latlng, flight-units
                 , flight-zone, hcoord, hcoord-utm, mtl, numbers, scientific
                 , siggy-chardust, smallcheck, stdenv, tasty, tasty-compare
                 , tasty-hunit, tasty-quickcheck, tasty-smallcheck, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-earth";
                   version = "0.1.0";
                   src = ../earth;
                   libraryHaskellDepends = [
                     aeson base bifunctors detour-via-sci detour-via-uom fgl
                     flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
                     numbers scientific siggy-chardust uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base bifunctors detour-via-sci detour-via-uom doctest fgl
                     flight-latlng flight-units flight-zone hcoord hcoord-utm mtl
                     numbers scientific siggy-chardust smallcheck tasty tasty-compare
                     tasty-hunit tasty-quickcheck tasty-smallcheck uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Distances on the WGS84 ellipsoid, the FAI sphere and the UTM projection";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-fsdb = dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, containers, detour-via-sci, doctest
                 , flight-comp, flight-gap, flight-latlng, flight-units, flight-zone
                 , hxt, hxt-pickle-utils, hxt-xpath, megaparsec, newtype, path
                 , scientific, split, statistics, stdenv, time, uom-plugin, vector
                 }:
                 mkDerivation {
                   pname = "flight-fsdb";
                   version = "0.1.0";
                   src = ../fsdb;
                   libraryHaskellDepends = [
                     aeson base containers detour-via-sci flight-comp flight-gap
                     flight-latlng flight-units flight-zone hxt hxt-xpath megaparsec
                     newtype path scientific split statistics time uom-plugin vector
                   ];
                   testHaskellDepends = [
                     aeson base containers detour-via-sci doctest flight-comp flight-gap
                     flight-latlng flight-units flight-zone hxt hxt-pickle-utils
                     hxt-xpath megaparsec newtype path scientific split statistics time
                     uom-plugin vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A parser for fsdb, the database XML format of FS";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-gap = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, cassava, containers, detour-via-sci
                 , detour-via-uom, doctest, flight-units, newtype, scientific
                 , siggy-chardust, smallcheck, statistics, stdenv, tasty
                 , tasty-hunit, tasty-quickcheck, tasty-smallcheck, template-haskell
                 , text, uom-plugin, vector
                 }:
                 mkDerivation {
                   pname = "flight-gap";
                   version = "0.1.0";
                   src = ../gap;
                   libraryHaskellDepends = [
                     aeson base cassava containers detour-via-sci detour-via-uom
                     flight-units newtype scientific siggy-chardust template-haskell
                     text uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base cassava containers detour-via-sci detour-via-uom doctest
                     flight-units newtype scientific siggy-chardust smallcheck
                     statistics tasty tasty-hunit tasty-quickcheck tasty-smallcheck
                     template-haskell text uom-plugin vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "GAP Scoring";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-igc = super.callPackage (
                 { mkDerivation, base, bytestring, doctest, flight-clip, megaparsec
                 , stdenv, tasty-quickcheck, time, utf8-string
                 }:
                 mkDerivation {
                   pname = "flight-igc";
                   version = "2.0.0";
                   src = ../igc;
                   libraryHaskellDepends = [
                     base bytestring flight-clip megaparsec tasty-quickcheck time
                     utf8-string
                   ];
                   testHaskellDepends = [
                     base bytestring doctest flight-clip megaparsec tasty-quickcheck
                     time utf8-string
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "A parser for IGC files";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-kml = super.callPackage (
                 { mkDerivation, aeson, base, detour-via-sci, doctest, flight-clip
                 , hxt, hxt-xpath, megaparsec, path, raw-strings-qq, siggy-chardust
                 , smallcheck, split, stdenv, tasty, tasty-hunit, tasty-quickcheck
                 , tasty-smallcheck, template-haskell, time
                 }:
                 mkDerivation {
                   pname = "flight-kml";
                   version = "1.1.0";
                   src = ../kml;
                   libraryHaskellDepends = [
                     aeson base detour-via-sci flight-clip hxt hxt-xpath megaparsec path
                     siggy-chardust split time
                   ];
                   testHaskellDepends = [
                     aeson base detour-via-sci doctest flight-clip hxt hxt-xpath
                     megaparsec path raw-strings-qq siggy-chardust smallcheck split
                     tasty tasty-hunit tasty-quickcheck tasty-smallcheck
                     template-haskell time
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
                   src = ../latlng;
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
                 , detour-via-sci, directory, filemanip, filepath, flight-clip
                 , flight-comp, flight-gap, flight-kml, flight-latlng, flight-mask
                 , flight-route, flight-zone, lens, mtl, path, scientific, split
                 , stdenv, time, unordered-containers, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-lookup";
                   version = "0.1.0";
                   src = ../lookup;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-clip flight-comp flight-gap flight-kml
                     flight-latlng flight-mask flight-route flight-zone lens mtl path
                     scientific split time unordered-containers uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition data access";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-mask = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, base, bytestring, cmdargs, containers
                 , detour-via-sci, directory, doctest, fgl, filepath, flight-clip
                 , flight-comp, flight-earth, flight-gap, flight-kml, flight-latlng
                 , flight-route, flight-scribe, flight-span, flight-task
                 , flight-track, flight-units, flight-zone, lens, mtl, numbers, path
                 , safe-exceptions, siggy-chardust, split, stdenv, these, time
                 , uom-plugin, yaml
                 }:
                 mkDerivation {
                   pname = "flight-mask";
                   version = "0.1.0";
                   src = ../mask;
                   libraryHaskellDepends = [
                     base bytestring cmdargs containers detour-via-sci directory fgl
                     filepath flight-clip flight-comp flight-earth flight-gap flight-kml
                     flight-latlng flight-route flight-scribe flight-span flight-task
                     flight-track flight-units flight-zone lens mtl numbers path
                     safe-exceptions siggy-chardust split these time uom-plugin yaml
                   ];
                   testHaskellDepends = [
                     base bytestring cmdargs containers detour-via-sci directory doctest
                     fgl filepath flight-clip flight-comp flight-earth flight-gap
                     flight-kml flight-latlng flight-route flight-scribe flight-span
                     flight-task flight-track flight-units flight-zone lens mtl numbers
                     path safe-exceptions siggy-chardust split these time uom-plugin
                     yaml
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Track logs masked by competition task zones";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-route = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, bifunctors, detour-via-sci
                 , flight-earth, flight-latlng, flight-task, flight-units
                 , flight-zone, hcoord-utm, numbers, scientific, siggy-chardust
                 , stdenv, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-route";
                   version = "0.1.0";
                   src = ../route;
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
                 , detour-via-sci, directory, filemanip, filepath, flight-clip
                 , flight-comp, flight-gap, flight-latlng, flight-route, flight-zone
                 , mtl, path, safe-exceptions, scientific, split, stdenv, text, time
                 , unordered-containers, vector, yaml
                 }:
                 mkDerivation {
                   pname = "flight-scribe";
                   version = "0.1.0";
                   src = ../scribe;
                   libraryHaskellDepends = [
                     aeson base bytestring cassava containers detour-via-sci directory
                     filemanip filepath flight-clip flight-comp flight-gap flight-latlng
                     flight-route flight-zone mtl path safe-exceptions scientific split
                     text time unordered-containers vector yaml
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
                   src = ../span;
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
                   src = ../task;
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
               flight-time = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, base, Cabal, Diff, directory, filepath
                 , flight-clip, flight-comp, flight-kml, flight-latlng
                 , flight-lookup, flight-mask, flight-scribe, lens, microlens, mtl
                 , prettyprinter, safe-exceptions, siggy-chardust, stdenv, tasty
                 , tasty-golden, text, these, time, transformers, uom-plugin
                 , utf8-string, vector
                 }:
                 mkDerivation {
                   pname = "flight-time";
                   version = "0.1.0";
                   src = ../time;
                   libraryHaskellDepends = [
                     base directory filepath flight-clip flight-comp flight-kml
                     flight-latlng flight-lookup flight-mask flight-scribe lens mtl
                     safe-exceptions siggy-chardust these time uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson base Cabal Diff directory filepath flight-clip flight-comp
                     flight-kml flight-latlng flight-lookup flight-mask flight-scribe
                     lens microlens mtl prettyprinter safe-exceptions siggy-chardust
                     tasty tasty-golden text these time transformers uom-plugin
                     utf8-string vector
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Align times of competing pilot's tracklogs";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
               flight-track = super.callPackage (
                 { mkDerivation, base, bytestring, containers, directory, doctest
                 , filepath, flight-clip, flight-comp, flight-igc, flight-kml, mtl
                 , path, split, stdenv, time, utf8-string
                 }:
                 mkDerivation {
                   pname = "flight-track";
                   version = "0.1.0";
                   src = ../track;
                   libraryHaskellDepends = [
                     base bytestring containers directory filepath flight-clip
                     flight-comp flight-igc flight-kml mtl path split time utf8-string
                   ];
                   testHaskellDepends = [
                     base bytestring containers directory doctest filepath flight-clip
                     flight-comp flight-igc flight-kml mtl path split time utf8-string
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Hang gliding and paragliding competition track logs";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               flight-units = dontHaddock (super.callPackage (
                 { mkDerivation, base, bifunctors, detour-via-sci, fixed, formatting
                 , newtype, numbers, siggy-chardust, stdenv, text, uom-plugin
                 }:
                 mkDerivation {
                   pname = "flight-units";
                   version = "0.1.0";
                   src = ../units;
                   libraryHaskellDepends = [
                     base bifunctors detour-via-sci fixed formatting newtype numbers
                     siggy-chardust text uom-plugin
                   ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Units used in hang gliding and paragliding competitions";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {});
               flight-zone = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, aeson, aeson-pretty, base, bytestring
                 , detour-via-sci, detour-via-uom, doctest, flight-latlng
                 , flight-units, here, newtype, scientific, siggy-chardust, stdenv
                 , tasty, tasty-discover, tasty-golden, tasty-hspec, text
                 , uom-plugin, yaml
                 }:
                 mkDerivation {
                   pname = "flight-zone";
                   version = "0.1.0";
                   src = ../zone;
                   libraryHaskellDepends = [
                     aeson base detour-via-sci detour-via-uom flight-latlng flight-units
                     newtype scientific siggy-chardust uom-plugin
                   ];
                   testHaskellDepends = [
                     aeson aeson-pretty base bytestring detour-via-sci detour-via-uom
                     doctest flight-latlng flight-units here newtype scientific
                     siggy-chardust tasty tasty-discover tasty-golden tasty-hspec text
                     uom-plugin yaml
                   ];
                   testToolDepends = [ tasty-discover ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Control zones to fly";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {}));
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
               hxt-xpath = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, base, containers, directory, filepath, hxt, parsec
                 , stdenv
                 }:
                 mkDerivation {
                   pname = "hxt-xpath";
                   version = "9.1.2.2";
                   sha256 = "50377cb5fc17a31091ef41d648cb26ce8d8bd52f9dc639e5b654b118804e9872";
                   libraryHaskellDepends = [
                     base containers directory filepath hxt parsec
                   ];
                   homepage = "https://github.com/UweSchmidt/hxt";
                   description = "The XPath modules for HXT";
                   license = "unknown";
                 }
                 ) {}));
               megaparsec = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, base, bytestring, case-insensitive, containers
                 , criterion, deepseq, hspec, hspec-expectations, mtl
                 , parser-combinators, QuickCheck, scientific, stdenv, text
                 , transformers, weigh
                 }:
                 mkDerivation {
                   pname = "megaparsec";
                   version = "7.0.4";
                   sha256 = "325ba5cee8cdef91e351fb2db0b38562f8345b0bcdfed97045671357501de8c1";
                   libraryHaskellDepends = [
                     base bytestring case-insensitive containers deepseq mtl
                     parser-combinators scientific text transformers
                   ];
                   testHaskellDepends = [
                     base bytestring case-insensitive containers hspec
                     hspec-expectations mtl parser-combinators QuickCheck scientific
                     text transformers
                   ];
                   benchmarkHaskellDepends = [
                     base containers criterion deepseq text weigh
                   ];
                   homepage = "https://github.com/mrkkrp/megaparsec";
                   description = "Monadic parser combinators";
                   license = stdenv.lib.licenses.bsd2;
                 }
                 ) {}));
               parser-combinators = dontCheck (dontHaddock (super.callPackage (
                 { mkDerivation, base, stdenv }:
                 mkDerivation {
                   pname = "parser-combinators";
                   version = "1.0.0";
                   sha256 = "e54c8d6071bc67866dffb661e5f56de6d632f40abdfe76b9f56a734ca76e8edf";
                   libraryHaskellDepends = [ base ];
                   homepage = "https://github.com/mrkkrp/parser-combinators";
                   description = "Lightweight package providing commonly useful parser combinators";
                   license = stdenv.lib.licenses.bsd3;
                 }
                 ) {}));
               siggy-chardust = super.callPackage (
                 { mkDerivation, base, doctest, smallcheck, stdenv, tasty
                 , tasty-hunit, tasty-quickcheck, tasty-smallcheck
                 }:
                 mkDerivation {
                   pname = "siggy-chardust";
                   version = "1.0.0";
                   src = ../siggy-chardust;
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
                   src = ../tasty-compare;
                   libraryHaskellDepends = [ base call-stack tasty tasty-hunit ];
                   homepage = "https://github.com/blockscope/flare-timing#readme";
                   description = "Tasty HUnit extensions for comparisons";
                   license = stdenv.lib.licenses.mpl20;
                 }
                 ) {};
               uom-plugin = doJailbreak (dontCheck (super.callPackage (
                 { mkDerivation, base, containers, deepseq, ghc, ghc-tcplugins-extra
                 , hlint, stdenv, tasty, tasty-hunit, template-haskell, units-parser
                 }:
                 mkDerivation {
                   pname = "uom-plugin";
                   version = "0.3.0.0";
                   sha256 = "94be3fdd1c162afec2c0c16a4ee280308d9c519cf5d061b105d426f211a24699";
                   libraryHaskellDepends = [
                     base containers deepseq ghc ghc-tcplugins-extra template-haskell
                     units-parser
                   ];
                   testHaskellDepends = [ base hlint tasty tasty-hunit ];
                   homepage = "https://github.com/adamgundry/uom-plugin#readme";
                   description = "Units of measure as a GHC typechecker plugin";
                   license = stdenv.lib.licenses.bsd3;
                 }
                 ) {}));
            };
          };
        };
      };

      # StandardPackage packages
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
    app-serve = pkgs.haskell.packages.${ghcver}.app-serve;
    detour-via-sci = pkgs.haskell.packages.${ghcver}.detour-via-sci;
    detour-via-uom = pkgs.haskell.packages.${ghcver}.detour-via-uom;
    doctest = pkgs.haskell.packages.${ghcver}.doctest;
    flare-timing = pkgs.haskell.packages.${ghcver}.flare-timing;
    flight-clip = pkgs.haskell.packages.${ghcver}.flight-clip;
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
    flight-time = pkgs.haskell.packages.${ghcver}.flight-time;
    flight-track = pkgs.haskell.packages.${ghcver}.flight-track;
    flight-units = pkgs.haskell.packages.${ghcver}.flight-units;
    flight-zone = pkgs.haskell.packages.${ghcver}.flight-zone;
    hcoord = pkgs.haskell.packages.${ghcver}.hcoord;
    hcoord-utm = pkgs.haskell.packages.${ghcver}.hcoord-utm;
    hxt-xpath = pkgs.haskell.packages.${ghcver}.hxt-xpath;
    megaparsec = pkgs.haskell.packages.${ghcver}.megaparsec;
    parser-combinators = pkgs.haskell.packages.${ghcver}.parser-combinators;
    siggy-chardust = pkgs.haskell.packages.${ghcver}.siggy-chardust;
    tasty-compare = pkgs.haskell.packages.${ghcver}.tasty-compare;
    uom-plugin = pkgs.haskell.packages.${ghcver}.uom-plugin;
  };

in projpkgs

# Usage:
#
#   For any of the primary packages defined above (e.g. app-serve),
#   one of the following can be done:
#
#   $ nix-build -A app-serve flare-timing-project.nix
#     <generates "result" link in local directory>
#
#   $ nix-build -A app-serve.env flare-timing-project.nix
#   nix-shell$ <dev environment for "cabal build">
#
#   $ git clone app-serve
#   $ cd app-serve
#   $ cat > shell.nix << EOF
#   { ghcver ? "ghc822" }:
#   (import /Users/pdejoux/dev/src/blockscope/flare-timing/vernix/flare-timing-project.nix { inherit ghcver; }).app-serve.env
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
