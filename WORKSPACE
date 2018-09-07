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
    revision = "5e65e5486bd71062e3eecf35f212018bfd621863",
)

nixpkgs_package(
    name = "ghc",
    build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
    nix_file_content = """
  let pkgs = import <nixpkgs> {};
      hlib = pkgs.haskell.lib;

      fgl-src = pkgs.fetchgit {
              url = "http://github.com/haskell/fgl.git";
              sha256 = "0biwsaj6s24l8ix95hkw4syl87ywxy363pr413kazzbhi0csf20s";
              rev = "e29503775e474b7a7cd8951b6bebcf1529b888b5";
            };

      hcoord-src = pkgs.fetchgit {
              url = "http://github.com/BlockScope/hcoord.git";
              rev = "bb17ddba271829f3902d9ae3af97f2723eb0ab47";
            };

      hp = pkgs.haskell.packages.ghc822.override {
            overrides = self: super: {
              fgl = super.callCabal2nix fgl-src {};
              hcoord = super.callCabal2nix (hcoord-src + "/hcoord") {};
              hcoord-utm = super.callCabal2nix (hcoord-src + "/hcoord-utm") {};
            };
        };

  in hp.ghcWithPackages (p: with p;
        [ aeson bifunctors cassava doctest fgl fixed formatting
          hcoord hxt hxt-xpath
          newtype numbers path
          scientific smallcheck split statistics
          tasty-hunit tasty-quickcheck template-haskell time
          uom-plugin utf8-string
        ])
  """,
    repository = "@nixpkgs",
)

register_toolchains("//:ghc")
