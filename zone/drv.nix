{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , detour-via-sci, detour-via-uom, doctest, flight-latlng
      , flight-units, here, newtype, scientific, siggy-chardust, stdenv
      , tasty, tasty-discover, tasty-golden, tasty-hspec, text
      , uom-plugin, yaml
      }:
      mkDerivation {
        pname = "flight-zone";
        version = "0.1.0";
        src = ./.;
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
