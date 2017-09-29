module Wods
    exposing
        ( Wod
        , WodType(..)
        , wods
        , getCardio
        , getPower
        , getEndurance
        , normalize
        )

import List exposing (foldl)
import String exposing (toInt)
import Result exposing (withDefault)
import Time exposing (Time)
import Date.Extra.Create exposing (timeFromFields)
import Date exposing (toTime)


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


wods : List Wod
wods =
    [ { id = "frn"
      , name = "Fran"
      , cardio = 0.3
      , endurance = 0.8
      , power = 0.3
      , range =
            ForTime
                { worst = toTime <| timeFromFields 0 2 0 0
                , best = toTime <| timeFromFields 0 0 0 0
                , value = Nothing
                }
      }
    , { id = "mrh"
      , name = "Murph"
      , cardio = 0.9
      , endurance = 0.3
      , power = 0.1
      , range = ForReps { worst = 0, best = 100, value = Nothing }
      }
    , { id = "sq"
      , name = "Best Squat"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range = ForReps { worst = 0, best = 100, value = Nothing }
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


normalize : WodType -> Maybe Int
normalize range =
    let
        ratio =
            case range of
                ForTime range ->
                    Maybe.map (\x -> (x - range.worst) / (range.best - range.worst)) range.value

                PRInfo range ->
                    Maybe.map (\x -> (toFloat <| x - range.worst) / (toFloat <| range.best - range.worst)) range.value

                ForReps range ->
                    Maybe.map (\x -> (toFloat <| x - range.worst) / (toFloat <| range.best - range.worst)) range.value
    in
        Maybe.map (\x -> round <| 100.0 * x) ratio


getFactor : (Wod -> Float) -> List Wod -> Int
getFactor factor wods =
    let
        weightedSum =
            let
                addWeightedValue w sum =
                    sum + (toFloat (Maybe.withDefault 0 (normalize w.range))) * (factor w)
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
