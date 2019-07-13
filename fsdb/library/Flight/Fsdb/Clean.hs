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
    , localPart
    , hasName
    , hasNameWith
    , none
    , processAttrl
    , isElem
    , when
    , seqA
    , filterA
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
                ])
        >>> writeDocumentToString [withIndent yes]

    return . maybe (Left "Couldn't filter FSDB.") Right . listToMaybe $ FsdbXml <$> xs

fsCompetitionNotes :: ArrowXml a => a XmlTree XmlTree
fsCompetitionNotes =
    processTopDown
        $ none `when` (isElem >>> hasName "FsCompetitionNotes")

-- <FsParticipant
--     id="101"
--     name="Davis Straub"
--     nat_code_3166_a3="USA"
--     female="0"
--     birthday="19XX-XX-XX"
--     glider="Wills Wing T2C 144"
--     glider_main_colors="Blue window"
--     sponsor="The Oz Report"
--     fai_licence="1"
--     CIVLID="XXXX" />
fsParticipant :: ArrowXml a => a XmlTree XmlTree
fsParticipant =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsParticipant")
            (processAttrl . filterA $ hasName "id" <+> hasName "name")
