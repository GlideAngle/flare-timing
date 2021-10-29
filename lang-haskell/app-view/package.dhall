let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions-ghcjs.dhall
    ⫽ { name =
          "app-view"
      , synopsis =
          "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
      , description =
          "Scoring and viewing hang gliding and paragliding competitions."
      , category =
          "Data, Parsing"
      , github =
          "blockscope/flare-timing/app-view"
      , ghc-options =
          [ "-Wall" ]
      , dependencies =
          defs.dependencies
      , executables =
          { comp-view =
              { dependencies =
                  [ "base >4.9 && <5"
                  , "aeson"
                  , "containers"
                  , "mtl"
                  , "reflex"
                  , "reflex-dom"
                  , "ghcjs-base"
                  , "ghcjs-dom"
                  , "scientific"
                  , "text"
                  , "time"
                  ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "App.hs"
              , source-dirs =
                  "comp-view"
              }
          }
      }
