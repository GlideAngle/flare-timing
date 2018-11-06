let
  lib = import ./package.nix;
in
  { main = "Rounding";
    src = ./test-suite-digits;
    packages = [ lib ];
    dependencies = [
      "tasty"
      "tasty-hunit"
      "tasty-quickcheck"
      "tasty-smallcheck"
      "smallcheck"
    ];
    extensions = [ "PackageImports"];
  }
