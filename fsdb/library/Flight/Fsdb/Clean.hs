module Flight.Fsdb.Clean (cleanComp) where

import Data.Maybe (listToMaybe)
import Text.XML.HXT.Core
    ( (>>>)
    , (<+>)
    , XmlTree
    , ArrowXml
    , runX
    , withValidate
    , withWarnings
    , withRemoveWS
    , withIndent
    , readString
    , writeDocumentToString
    , processChildren
    , processTopDown
    , no
    , yes
    , hasName
    , none
    , processAttrl
    , isElem
    , when
    , seqA
    , filterA
    , removeAttr
    )

import Flight.Comp (FsdbXml(..))

-- | Cleans the *.fsdb, removing any potentially sensitive personal information
-- such as birthdays and phone numbers or messages left in comments.
cleanComp :: FsdbXml -> IO (Either String FsdbXml)
cleanComp (FsdbXml contents) = do
    let doc =
            readString
                [ withValidate no
                , withWarnings no
                , withRemoveWS yes
                ]
                contents

    xs <- runX
        $ doc
        >>> (processChildren . seqA $
                [ fsCompetitionNotes
                , fsParticipant
                , fsTimeStamp
                ])
        >>> writeDocumentToString
                [ withIndent yes
                -- WARNING: I have not been able to output the processing
                -- instruction without getting encoding problems such as
                -- Igor Eržen encoded as Igor ErÅ¾en.
                -- NOTE: Need both withXmlPi and withOutputEncoding to get:
                -- <?xml version="1.0" encoding="UTF-8"?>
                -- , withXmlPi yes
                -- , withOutputEncoding utf8
                ]

    return . maybe (Left "Couldn't filter FSDB.") Right . listToMaybe $ FsdbXml <$> xs

fsCompetitionNotes :: ArrowXml a => a XmlTree XmlTree
fsCompetitionNotes =
    processTopDown
        $ none `when` (isElem >>> hasName "FsCompetitionNotes")

fsParticipant :: ArrowXml a => a XmlTree XmlTree
fsParticipant =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsParticipant")
            (processAttrl . filterA $ hasName "id" <+> hasName "name")

-- | The FsFlightData and FsResult elements of an *.fsdb have ts "time stamp"
-- attributes . Having these gets in the way of doing a file comparison. Two
-- *.fsdb files for the same competition that have been rescored will have many
-- ts attribute differences, up to two changes per-pilot per-task. The clean
-- and trim files are typically source controlled and as such will have commit
-- times. This function drops timestamp attributes from FsFlightData and
-- FsResult elements.
fsTimeStamp :: ArrowXml a => a XmlTree XmlTree
fsTimeStamp =
    processTopDown
        $ (flip when)
            (isElem >>> (hasName "FsFlightData" <+> hasName "FsResult"))
            (removeAttr "ts")
