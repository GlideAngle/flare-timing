let
  lib =
    { src = ./library;
      dependencies = [ "base" ];
      extensions = [ "PackageImports"];
    };
in
  { main = "DocTest";
    src = ./test-suite-doctest;
    packages = [ lib ];
    dependencies = [ "base" "doctest" ];
    extensions = [ "PackageImports"];
  }
