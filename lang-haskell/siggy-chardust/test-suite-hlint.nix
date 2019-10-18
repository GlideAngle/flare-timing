let
  lib = import ./package.nix;
in
  { main = "HLint";
    src = ./test-suite-hlint;
    packages = [ lib ];
    dependencies = [ "hlint" ];
    extensions = [ "PackageImports"];
  }
