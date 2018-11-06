{ src = ./library;
  dependencies = [
    "base"
    "newtype"
    "scientific"
    "aeson"
    "cassava"
    "template-haskell"
    "siggy-chardust"
  ];
  extensions = [ "PackageImports" ];
}
