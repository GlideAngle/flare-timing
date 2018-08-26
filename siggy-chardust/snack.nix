let
  lib =
    { src = ./library;
      dependencies = ["base >=4.9.1.0"];
      extensions = ["PackageImports"];
    };
in
  { main = "Rounding";
    src = ./test-suite-digits;
    packages = [lib ;
    dependencies =
      [ "tasty"
        "tasty-hunit"
        "tasty-quickcheck"
        "tasty-smallcheck"
        "smallcheck"
      ];
  }
