
let
  lib =
    { src = ./library;
      dependencies = [ "base" ];
      extensions = [ "PackageImports"];
    };
in
  { main = "Rounding";
    src = ./test-suite-digits;
    packages = [ lib ];
    dependencies = [
      "base"
      "tasty"
      "tasty-hunit"
      "tasty-quickcheck"
      "tasty-smallcheck"
      "smallcheck"
    ];
    extensions = [ "PackageImports"];
  }
