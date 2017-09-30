module Wods
    exposing
        ( Wod
        , WodType(..)
        , wods
        , getCardio
        , getPower
        , getEndurance
        , getTotalEstimation
        , normalize
        )

import List exposing (foldl)
import String exposing (toInt)
import Result exposing (withDefault)
import Time exposing (Time)
import Date.Extra.Create exposing (timeFromFields)
import Date exposing (toTime)
import Arithmetic exposing (cubeRoot)


type alias RangeInt =
    { worst : Int
    , best : Int
    , value : Maybe Int
    }


type alias RangeTime =
    { worst : Time
    , best : Time
    , value : Maybe Time
    }


type WodType
    = ForTime RangeTime
    | PRInfo RangeInt
    | ForReps RangeInt


type alias Wod =
    { id : String
    , name : String
    , cardio : Float
    , endurance : Float
    , power : Float
    , range : WodType
    }



--- List of WODs ---


wods : List Wod
wods =
    [ { id = "frn"
      , name = "Fran"
      , cardio = 0.3
      , endurance = 0.8
      , power = 0.3
      , range =
            ForTime
                { worst = toTime <| timeFromFields 0 5 0 0
                , best = toTime <| timeFromFields 0 2 1 0
                , value = Nothing
                }
      }
    , { id = "mrh"
      , name = "Murph"
      , cardio = 0.9
      , endurance = 0.3
      , power = 0.1
      , range =
            ForTime
                { worst = toTime <| timeFromFields 0 60 0 0
                , best = toTime <| timeFromFields 0 30 0 0
                , value = Nothing
                }
      }
    , { id = "sq"
      , name = "Back Squat"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range = PRInfo { worst = 40, best = 215, value = Nothing }
      }
    , { id = "clj"
      , name = "Clean and Jerk"
      , cardio = 0.1
      , endurance = 0.4
      , power = 1.0
      , range = PRInfo { worst = 20, best = 170, value = Nothing }
      }
    , { id = "sntch"
      , name = "Snatch"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range = PRInfo { worst = 15, best = 140, value = Nothing }
      }
    , { id = "dlft"
      , name = "Deadlift"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range = PRInfo { worst = 60, best = 260, value = Nothing }
      }
    , { id = "fgb"
      , name = "Fight Gone Bad"
      , cardio = 0.9
      , endurance = 0.6
      , power = 0.3
      , range = ForReps { worst = 200, best = 508, value = Nothing }
      }
    , { id = "plps"
      , name = "Max Pull-ups"
      , cardio = 0.9
      , endurance = 0.6
      , power = 0.3
      , range = ForReps { worst = 5, best = 75, value = Nothing }
      }
    , { id = "grc"
      , name = "Grace"
      , cardio = 0.3
      , endurance = 0.9
      , power = 0.7
      , range =
            ForTime
                { worst = toTime <| timeFromFields 0 6 0 0
                , best = toTime <| timeFromFields 0 1 11 0
                , value = Nothing
                }
      }
    ]


getCardio : List Wod -> Int
getCardio wods =
    getFactor .cardio wods


getPower : List Wod -> Int
getPower wods =
    getFactor .power wods


getEndurance : List Wod -> Int
getEndurance wods =
    getFactor .endurance wods


getTotalEstimation : List Wod -> Int
getTotalEstimation wods =
    round <|
        cubeRoot <|
            toFloat <|
                (getCardio wods)
                    * (getPower wods)
                    * (getEndurance wods)


normalize : WodType -> Maybe Int
normalize range =
    let
        ratio =
            case range of
                ForTime range ->
                    Maybe.map
                        (\x -> (x - range.worst) / (range.best - range.worst))
                        range.value

                PRInfo range ->
                    Maybe.map
                        (\x ->
                            (toFloat <| x - range.worst)
                                / (toFloat <| range.best - range.worst)
                        )
                        range.value

                ForReps range ->
                    Maybe.map
                        (\x ->
                            (toFloat <| x - range.worst)
                                / (toFloat <| range.best - range.worst)
                        )
                        range.value
    in
        Maybe.map (\x -> round <| 100.0 * (clamp 0.0 1.0 x)) ratio


getFactor : (Wod -> Float) -> List Wod -> Int
getFactor factor wods =
    let
        weightedSum =
            let
                addWeightedValue w sum =
                    sum
                        + (toFloat (Maybe.withDefault 0 (normalize w.range)))
                        * (factor w)
            in
                foldl addWeightedValue 0 wods

        sumOfWeights =
            foldl
                (\w sum ->
                    if normalize w.range /= Nothing then
                        sum + factor w
                    else
                        sum
                )
                0.0
                wods
    in
        round (weightedSum / sumOfWeights)
