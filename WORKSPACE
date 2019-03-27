workspace(name = "flare_timing")

RULES_HASKELL_VERSION = "18a37091a23806ad3dd42867b3ec62b9b4b4eaea"
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-{}".format(RULES_HASKELL_VERSION),
    urls = ["https://github.com/tweag/rules_haskell/archive/{}.tar.gz".format(RULES_HASKELL_VERSION)],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.2.3",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.3.tar.gz"],
    sha256 = "2647bc9d5476fba95d9b4cc300be1ba9ad353e4e33bee01e041886aa4f4b554a",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_package",
)

nixpkgs_git_repository(
    name = "nixpkgs",
    remote = "https://github.com/BlockScope/nixpkgs",
    revision = "167aa8ae3e58a545864ba7e23591ead65ab793fd",
)

nixpkgs_package(
    name = "ghc",
    build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
    nix_file_content = """
  let compiler = "ghc822";

      hostPkgs = import <nixpkgs> {};

      pinnedVersion = 
        {
          url = "https://github.com/BlockScope/nixpkgs";
          rev = "167aa8ae3e58a545864ba7e23591ead65ab793fd";
          sha256 = "1s27zh7k36vlidy61wrlp3ck6s872qwyk1v4p12c15r1bn6lirhq";
        };

      pinnedPkgs = hostPkgs.fetchgit {
        inherit (pinnedVersion) url rev sha256;
      };

      hcoord-drv =
        { mkDerivation, base, data-default, fetchgit, hlint, HUnit, ieee754
        , mtl, stdenv
        }:
        mkDerivation {
          pname = "hcoord";
          version = "2.1.0";
          src = fetchgit {
            url = "http://github.com/BlockScope/hcoord.git";
            sha256 = "0267n694m08bv73ld7f5flb66h3dxc7xgrbmkr757q0g87l8ndzq";
            rev = "3c3859dac5da111e57a6de09764ffdb127197c4a";
          };
          postUnpack = "sourceRoot+=/hcoord; echo source root reset to $sourceRoot";
          libraryHaskellDepends = [ base mtl ];
          testHaskellDepends = [ base data-default hlint HUnit ieee754 mtl ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/danfran/hcoord#readme";
          description = "Short synopsis";
          license = stdenv.lib.licenses.bsd3;
        };

    hcoord-utm-drv =
        { mkDerivation, base, data-default, fetchgit, hcoord, hlint, HUnit
        , ieee754, mtl, stdenv
        }:
        mkDerivation {
          pname = "hcoord-utm";
          version = "2.1.0";
          src = fetchgit {
            url = "http://github.com/BlockScope/hcoord.git";
            sha256 = "0267n694m08bv73ld7f5flb66h3dxc7xgrbmkr757q0g87l8ndzq";
            rev = "3c3859dac5da111e57a6de09764ffdb127197c4a";
          };
          postUnpack = "sourceRoot+=/hcoord-utm; echo source root reset to $sourceRoot";
          libraryHaskellDepends = [ base hcoord mtl ];
          testHaskellDepends = [
            base data-default hcoord hlint HUnit ieee754 mtl
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/danfran/hcoord#readme";
          description = "Short synopsis";
          license = stdenv.lib.licenses.bsd3;
        };

      megaparsec-drv =
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
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/mrkkrp/megaparsec";
          description = "Monadic parser combinators";
          license = stdenv.lib.licenses.bsd2;
        };

      hcoord = pkgs.haskellPackages.callPackage hcoord-drv {};
      hcoord-utm = pkgs.haskellPackages.callPackage hcoord-utm-drv {};
      megaparsec = pkgs.haskellPackages.callPackage megaparsec-drv {};

      config =
        {
          allowUnsupportedSystem = true;
          allowUnfree = true;

          packageOverrides = pkgs:
            let old = pkgs.haskell.packages.${compiler}; in rec {
            haskellPackages = pkgs.haskell.packages.${compiler}.override {
              overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super:
              {

                hcoord = super.callPackage hcoord-drv {};
                hcoord-utm = super.callPackage hcoord-utm-drv {};
                megaparsec = super.callPackage megaparsec-drv {};
              });
            };
          };
        };

      pkgs = import pinnedPkgs { inherit config; };

  in pkgs.haskell.packages.ghc822.ghcWithPackages (p: with p;
        [ aeson
          bifunctors
          cassava cmdargs
          detour-via-sci doctest
          fgl filemanip fixed formatting
          hcoord hcoord-utm hxt hxt-xpath
          lens
          megaparsec mtl
          newtype numbers
          path
          raw-strings-qq
          safe-exceptions scientific servant servant-server
          siggy-chardust smallcheck split statistics system-filepath
          tasty-hunit tasty-quickcheck template-haskell these time transformers
          uom-plugin utf8-string
          wai wai-cors
          yaml
        ])
  """,
    repository = "@nixpkgs",
)

register_toolchains("//:ghc")
