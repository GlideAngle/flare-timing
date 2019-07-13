module Flight.Fsdb.Filter (filterComp) where

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

filterComp :: FsdbXml -> IO (Either String FsdbXml)
filterComp (FsdbXml contents) = do
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
                [ fs
                , fsCompetition
                , fsCompetitionNotes
                , fsParticipant
                ])
        >>> writeDocumentToString [withIndent yes]

    return . maybe (Left "Couldn't filter FSDB.") Right . listToMaybe $ FsdbXml <$> xs

-- <Fs version="3.4"
--     comment="Supports only a single Fs element in a .fsdb file which must be the root element." />
fs :: ArrowXml a => a XmlTree XmlTree
fs =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "Fs")
            (processAttrl . filterA $ hasName "version")

fsCompetitionNotes :: ArrowXml a => a XmlTree XmlTree
fsCompetitionNotes =
    processTopDown
        $ none `when` (isElem >>> hasName "FsCompetitionNotes")

-- <FsCompetition
--     id="0"
--     name="QuestAir Open"
--     location="Groveland, Florida, USA"
--     from="2016-05-07"
--     to="2016-05-13"
--     utc_offset="-4"
--     discipline="hg"
--     ftv_factor="0" />
fsCompetition :: ArrowXml a => a XmlTree XmlTree
fsCompetition =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsCompetition")
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "discipline"
                    , "name"
                    , "location"
                    , "from"
                    , "to"
                    , "utc_offset"
                    ])
                . localPart)

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
