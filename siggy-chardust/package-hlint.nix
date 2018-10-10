let
  lib =
    { src = ./library;
      dependencies = [ "base" ];
      extensions = [ "PackageImports"];
    };
in
  { main = "HLint";
    src = ./test-suite-hlint;
    packages = [ lib ];
    dependencies = [ "hlint" ];
    extensions = [ "PackageImports"];
  }
