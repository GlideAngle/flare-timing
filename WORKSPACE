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
    revision = "4c0f61251769032b28102396483db63f72d4c2b1",
)

nixpkgs_package(
    name = "ghc",
    build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
    nix_file_content = """
  let pkgs = import <nixpkgs> { }; in
  pkgs.haskell.packages.ghc822.ghcWithPackages (p: with p;
    [ aeson bifunctors cassava fixed formatting newtype numbers scientific
      smallcheck tasty-quickcheck template-haskell uom-plugin utf8-string
    ]
  )
  """,
    repository = "@nixpkgs",
)

register_toolchains("//:ghc")
