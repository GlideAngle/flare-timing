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
                , fsScoreFormula
                , fsParticipant
                , fsFlightData
                , fsResult
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

-- <FsScoreFormula
--     id="GAP2015"
--     min_dist="5"
--     nom_dist="60"
--     nom_time="2"
--     nom_launch="1"
--     nom_goal="0.2"
--     day_quality_override="0"
--     bonus_gr="5"
--     jump_the_gun_factor="3"
--     jump_the_gun_max="300"
--     normalize_1000_before_day_quality="0"
--     time_points_if_not_in_goal="0.8"
--     use_1000_points_for_max_day_quality="0"
--     use_arrival_position_points="1"
--     use_arrival_time_points="0"
--     use_departure_points="0"
--     use_difficulty_for_distance_points="1"
--     use_distance_points="1"
--     use_distance_squared_for_LC="1"
--     use_leading_points="1"
--     use_semi_circle_control_zone_for_goal_line="0"
--     use_time_points="1"
--     final_glide_decelerator="none"
--     no_final_glide_decelerator_reason=""
--     min_time_span_for_valid_task="60"
--     score_back_time="15" />
fsScoreFormula :: ArrowXml a => a XmlTree XmlTree
fsScoreFormula =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsScoreFormula")
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "id"
                    , "min_dist"
                    , "nom_dist"
                    , "nom_time"
                    , "nom_launch"
                    , "nom_goal"
                    , "score_back_time"
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

-- <FsFlightData
--     distance="47.762"
--     bonus_distance="48.282"
--     started_ss="2016-05-07T13:50:26-04:00"
--     finished_ss=""
--     altitude_at_ess="0"
--     finished_task="2016-05-07T14:39:09-04:00"
--     tracklog_filename="t1.straub.101.IGC"
--     lc="5.52514210861473"
--     iv="224993640"
--     ts="2019-07-09T23:47:10+12:00"
--     alt="133"
--     bonus_alt="144"
--     max_alt="1832"
--     last_tracklog_point_distance="47.353"
--     bonus_last_tracklog_point_distance="47.368"
--     last_tracklog_point_time="2016-05-07T14:41:58-04:00"
--     last_tracklog_point_alt="40"
--     landed_before_deadline="1" />
fsFlightData :: ArrowXml a => a XmlTree XmlTree
fsFlightData =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsFlightData")
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "tracklog_filename"
                    ])
                . localPart)

-- <FsResult
--     rank="40"
--     points="121"
--     distance="47.762"
--     ss_time="00:00:00"
--     finished_ss_rank="19"
--     distance_points="121.2"
--     linear_distance_points="68.66"
--     difficulty_distance_points="52.54"
--     time_points="0"
--     arrival_points="0"
--     departure_points="0"
--     leading_points="0"
--     penalty="0"
--     penalty_points="0"
--     penalty_reason=""
--     penalty_points_auto="0"
--     penalty_reason_auto=""
--     penalty_min_dist_points="0"
--     got_time_but_not_goal_penalty="False"
--     started_ss="2016-05-07T13:50:00-04:00"
--     ss_time_dec_hours="0"
--     ts="2019-07-09T23:47:14+12:00"
--     real_distance="47.762"
--     last_distance="47.353"
--     last_altitude_above_goal="0"
--     altitude_bonus_seconds="0"
--     altitude_bonus_time="00:00:00"
--     altitude_at_ess="0"
--     scored_ss_time="00:00:00"
--     landed_before_stop="False" />
fsResult :: ArrowXml a => a XmlTree XmlTree
fsResult =
    processTopDown
        $ (flip when)
            (isElem >>> hasName "FsResult")
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "rank"
                    , "points"
                    , "distance"
                    , "ss_time"
                    , "distance_points"
                    , "linear_distance_points"
                    , "difficulty_distance_points"
                    , "time_points"
                    , "arrival_points"
                    , "leading_points"
                    , "real_distance"
                    , "last_distance"
                    ])
                . localPart)
