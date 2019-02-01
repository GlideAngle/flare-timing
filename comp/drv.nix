{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cassava, containers
      , detour-via-sci, detour-via-uom, directory, doctest, filemanip
      , filepath, flight-clip, flight-earth, flight-gap, flight-latlng
      , flight-route, flight-units, flight-zone, lens, mtl, path
      , scientific, smallcheck, split, stdenv, tasty, tasty-hunit
      , tasty-quickcheck, tasty-smallcheck, text, time
      , unordered-containers, uom-plugin, vector
      }:
      mkDerivation {
        pname = "flight-comp";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring cassava containers detour-via-sci
          detour-via-uom directory filemanip filepath flight-clip
          flight-earth flight-gap flight-latlng flight-route flight-units
          flight-zone lens mtl path scientific split text time
          unordered-containers uom-plugin vector
        ];
        testHaskellDepends = [
          aeson base bytestring cassava containers detour-via-sci
          detour-via-uom directory doctest filemanip filepath flight-clip
          flight-earth flight-gap flight-latlng flight-route flight-units
          flight-zone lens mtl path scientific smallcheck split tasty
          tasty-hunit tasty-quickcheck tasty-smallcheck text time
          unordered-containers uom-plugin vector
        ];
        homepage = "https://github.com/blockscope/flare-timing#readme";
        description = "Hang gliding and paragliding competition scoring inputs";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
