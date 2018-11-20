let
  lib = import ./package.nix;
in
  { main = "DocTest";
    src = ./test-suite-doctest;
    packages = [ lib ];
    dependencies = [ "doctest" ];
    extensions = [ "PackageImports"];
  }
