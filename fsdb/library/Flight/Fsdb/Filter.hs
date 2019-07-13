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
    , hasName
    , none
    , processAttrl
    , isElem
    , when
    , whenNot
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
                , fsCompetitionNotes
                , fsParticipant
                ])
        >>> writeDocumentToString [withIndent yes]

    return . maybe (Left "Couldn't filter FSDB.") Right . listToMaybe $ FsdbXml <$> xs

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

fsParticipant :: ArrowXml a => a XmlTree XmlTree
fsParticipant =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsParticipant")
            (processAttrl . filterA $ hasName "id" <+> hasName "name")
