module Flight.Fsdb.Trim (trimComp) where

import Data.Maybe (listToMaybe)
import Text.XML.HXT.Core
    ( (>>>)
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
    , processAttrl
    , isElem
    , when
    , seqA
    , filterA
    )

import Flight.Comp (FsdbXml(..))
import Flight.Fsdb.Internal.Xml
    (fsCompetitionNotes, fsCompetitionResults, fsCustomAttributes, fsParticipant)

-- | Trims the XML of the *.fsdb leaving only inputs to scoring by flare-timing
-- or outputs from the scoring made by FS that flare-timing can compare with.
-- In any other scoring program can be made to work with this subset of the
-- *.fsdb format then flare-timing can be used to check the resultant scores.
trimComp :: FsdbXml -> IO (Either String FsdbXml)
trimComp (FsdbXml contents) = do
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
                , fsCompetitionResults
                , fsScoreFormula
                , fsParticipant
                , fsCustomAttributes
                , fsFlightData
                , fsResultPenalty
                , fsResult
                , fsTaskScoreParams
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

-- <Fs version="3.4"
--     comment="Supports only a single Fs element in a .fsdb file which must be the root element." />
fs :: ArrowXml a => a XmlTree XmlTree
fs =
    processTopDown
        $ when
            (processAttrl . filterA $ hasName "version")
            (isElem >>> hasName "Fs")

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
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "id"
                    , "discipline"
                    , "name"
                    , "location"
                    , "from"
                    , "to"
                    , "utc_offset"
                    ])
                . localPart)
            (isElem >>> hasName "FsCompetition")

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
--     score_back_time="15"
--     time_points_if_not_in_goal="0.8" />
--
-- <FsScoreFormula
--     id="GAP2020"
--     min_dist="7
--     nom_dist="60
--     nom_time="1.5
--     nom_launch="0.96
--     nom_goal="0.3
--     day_quality_override="0
--     bonus_gr="5
--     jump_the_gun_factor="2
--     jump_the_gun_max="300
--     normalize_1000_before_day_quality="0
--     time_points_if_not_in_goal="0.8
--     use_1000_points_for_max_day_quality="0
--     use_arrival_position_points="1
--     use_arrival_time_points="0
--     use_departure_points="0
--     use_difficulty_for_distance_points="1
--     use_distance_points="1
--     use_distance_squared_for_LC="1
--     use_leading_points="1
--     use_semi_circle_control_zone_for_goal_line="1
--     use_time_points="1
--     scoring_altitude="QNH
--     final_glide_decelerator="none
--     no_final_glide_decelerator_reason="
--     min_time_span_for_valid_task="45
--     score_back_time="15
--     use_proportional_leading_weight_if_nobody_in_goal="0
--     leading_weight_factor="1
--     turnpoint_radius_tolerance="0.001
--     turnpoint_radius_minimum_absolute_tolerance="5
--     number_of_decimals_task_results="1
--     number_of_decimals_competition_results="0
--     redistribute_removed_time_points_as_distance_points="1
--     use_best_score_for_ftv_validity="1
--     use_constant_leading_weight="0
--     use_pwca2019_for_lc="0
--     use_flat_decline_of_timepoints="1" />
fsScoreFormula :: ArrowXml a => a XmlTree XmlTree
fsScoreFormula =
    processTopDown
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "id"
                    , "min_dist"
                    , "nom_dist"
                    , "nom_time"
                    , "nom_launch"
                    , "nom_goal"
                    , "score_back_time"

                    , "jump_the_gun_factor"
                    , "jump_the_gun_max"

                    , "use_leading_points"
                    , "double_leading_weight"
                    , "use_arrival_position_points"
                    , "use_arrival_time_points"
                    , "use_distance_squared_for_LC"
                    , "time_points_if_not_in_goal"

                    , "use_flat_decline_of_timepoints"
                    ])
                . localPart)
            (isElem >>> hasName "FsScoreFormula")

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
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "tracklog_filename"
                    , "iv"
                    , "lc"
                    , "distance"
                    , "bonus_distance"
                    , "started_ss"
                    , "finished_ss"
                    ])
                . localPart)
            (isElem >>> hasName "FsFlightData")

-- <FsResultPenalty
--     penalty="0"
--     penalty_points="0"
--     penalty_reason="" />
fsResultPenalty :: ArrowXml a => a XmlTree XmlTree
fsResultPenalty =
    processTopDown
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "penalty"
                    , "penalty_points"
                    , "penalty_reason"
                    ])
                . localPart)
            (isElem >>> hasName "FsResultPenalty")

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
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "rank"
                    , "points"

                    , "linear_distance_points"
                    , "difficulty_distance_points"

                    , "distance_points"
                    , "leading_points"
                    , "arrival_points"
                    , "time_points"

                    , "real_distance"
                    , "distance"
                    , "last_distance"

                    , "started_ss"
                    , "finished_ss"
                    , "ss_time"

                    , "penalty_points_auto"
                    , "penalty_reason_auto"
                    ])
                . localPart)
            (isElem >>> hasName "FsResult")

-- <FsTaskScoreParams
--     ss_distance="150.41"
--     task_distance="160.044"
--     launch_to_ess_distance="160.044"
--     no_of_pilots_present="43"
--     no_of_pilots_flying="43"
--     no_of_pilots_lo="25"
--     no_of_pilots_reaching_nom_dist="37"
--     no_of_pilots_reaching_es="18"
--     no_of_pilots_reaching_goal="18"
--     sum_flown_distance="4954.745"
--     best_dist="160.044"
--     best_time="3.46694444444444"
--     worst_time="4.10222222222222"
--     no_of_pilots_in_competition="43"
--     no_of_pilots_landed_before_stop="0"
--     sum_dist_over_min="4739.745"
--     sum_real_dist_over_min="4739.745"
--     sum_flown_distances="4954.745"
--     best_real_dist="160.044"
--     last_start_time="2016-05-07T14:30:00-04:00"
--     first_start_time="2016-05-07T13:50:00-04:00"
--     first_finish_time="2016-05-07T17:18:01-04:00"
--     max_time_to_get_time_points="5.32891771075211"
--     no_of_pilots_with_time_points="18"
--     goalratio="0.418604651162791"
--     arrival_weight="0.0674832058812432"
--     departure_weight="0"
--     leading_weight="0.0944764882337404"
--     time_weight="0.377905952934962"
--     distance_weight="0.460134352950055"
--     smallest_leading_coefficient="2.16684339766897"
--     available_points_distance="460.134352950055"
--     available_points_time="377.905952934962"
--     available_points_departure="0"
--     available_points_leading="94.4764882337404"
--     available_points_arrival="67.4832058812432"
--     time_validity="1"
--     launch_validity="1"
--     distance_validity="1"
--     stop_validity="1"
--     day_quality="1"
--     ftv_day_validity="1"
--     time_points_stop_correction="0" />
fsTaskScoreParams :: ArrowXml a => a XmlTree XmlTree
fsTaskScoreParams =
    processTopDown
        $ when
            (processAttrl . filterA . hasNameWith $
                ( `elem`
                    [ "ss_distance"
                    , "task_distance"
                    , "launch_to_ess_distance"

                    , "no_of_pilots_present"
                    , "no_of_pilots_flying"
                    , "no_of_pilots_lo"
                    , "no_of_pilots_reaching_es"
                    , "no_of_pilots_landed_before_stop"
                    , "goal_ratio"

                    , "best_time"
                    , "best_dist"
                    , "best_real_dist"

                    , "sum_real_dist_over_min"

                    , "goalratio"
                    , "arrival_weight"
                    , "leading_weight"
                    , "time_weight"
                    , "distance_weight"

                    , "time_validity"
                    , "launch_validity"
                    , "distance_validity"
                    , "stop_validity"
                    , "day_quality"
                    ])
                . localPart)
            (isElem >>> hasName "FsTaskScoreParams")
