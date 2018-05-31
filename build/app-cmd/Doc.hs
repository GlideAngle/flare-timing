module Doc (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )

docFor :: String -> String
docFor x =
    "stack exec -- haddock --html --hyperlinked-source --odir=__docs/" ++ x

cleanRules :: Rules ()
cleanRules =
    phony "clean-docs" $
        removeFilesAfter "__docs" [ "//*" ] 

-- NOTE: Stack doesn't build docs for executables.
-- SEE: https://github.com/commercialhaskell/stack/issues/729
-- SEE: https://www.reddit.com/r/haskell/comments/5ugm9s/how_to_generate_haddock_docs_for_nonlibrary_code/
buildRules :: Rules ()
buildRules = do
    phony "docs" $
        need [ "extract-input-docs"
             , "task-length-docs"
             , "cross-zone-docs"
             , "tag-zone-docs"
             , "align-time-docs"
             , "discard-further-docs"
             , "mask-track-docs"
             , "land-out-docs"
             , "gap-point-docs"
             , "fsdb-parser-docs"
             , "igc-parser-docs"
             , "kml-parser-docs"
             ]

    phony "extract-input-docs" $
        cmd
            Shell
            (docFor "extract-input")
            [ "flare-timing/prod-apps/extract-input/ExtractInputOptions.hs" ]

    phony "task-length-docs" $
        cmd
            Shell
            (docFor "task-length")
            [ "flare-timing/prod-apps/task-length/TaskLengthOptions.hs" ]

    phony "cross-zone-docs" $
        cmd
            Shell
            (docFor "cross-zone")
            [ "flare-timing/prod-apps/cross-zone/CrossZoneOptions.hs" ]

    phony "tag-zone-docs" $
        cmd
            Shell
            (docFor "tag-zone")
            [ "flare-timing/prod-apps/tag-zone/TagZoneOptions.hs" ]

    phony "align-time-docs" $
        cmd
            Shell
            (docFor "align-time")
            [ "flare-timing/prod-apps/align-time/AlignTimeOptions.hs" ]

    phony "discard-further-docs" $
        cmd
            Shell
            (docFor "discard-further")
            [ "flare-timing/prod-apps/discard-further/DiscardFurtherOptions.hs" ]

    phony "mask-track-docs" $
        cmd
            Shell
            (docFor "mask-track")
            [ "flare-timing/prod-apps/mask-track/MaskTrackOptions.hs" ]

    phony "land-out-docs" $
        cmd
            Shell
            (docFor "land-out")
            [ "flare-timing/prod-apps/land-out/LandOutOptions.hs" ]

    phony "gap-point-docs" $
        cmd
            Shell
            (docFor "gap-point")
            [ "flare-timing/prod-apps/gap-point/GapPointOptions.hs" ]

    phony "fsdb-parser-docs" $
        cmd
            Shell
            (docFor "fsdb-parser")
            [ "flare-timing/test-apps/fsdb-parser/FsdbOptions.hs" ]

    phony "igc-parser-docs" $
        cmd
            Shell
            (docFor "igc-parser")
            [ "flare-timing/test-apps/igc-parser/IgcOptions.hs" ]

    phony "kml-parser-docs" $
        cmd
            Shell
            (docFor "kml-parser")
            [ "flare-timing/test-apps/kml-parser/KmlOptions.hs" ]
