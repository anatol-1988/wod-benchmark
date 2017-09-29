module Wods
    exposing
        ( Wod(..)
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


type alias WodProperties =
    { id : String
    , name : String
    , cardio : Float
    , endurance : Float
    , power : Float
    }


type alias RangeInt =
    { worst : Int
    , best : Int
    }


type alias RangeTime =
    { worst : Time
    , best : Time
    }


type Wod
    = ForTime WodProperties RangeTime Time
    | PRInfo WodProperties RangeInt Int
    | ForReps WodProperties RangeInt Int


wods : List Wod
wods =
    [ ForTime
        { id = "frn"
        , name = "Fran"
        , cardio = 0.3
        , endurance = 0.8
        , power = 0.3
        }
        { worst = toTime <| timeFromFields 0 2 0 0, best = toTime <| timeFromFields 0 0 0 0 }
        50
    , ForReps
        { id = "mrh"
        , name = "Murph"
        , cardio = 0.9
        , endurance = 0.3
        , power = 0.1
        }
        { worst = 0, best = 100 }
        50
    , PRInfo
        { id = "sq"
        , name = "Best Squat"
        , cardio = 0.1
        , endurance = 0.3
        , power = 0.9
        }
        { worst = 0, best = 100 }
        50
    ]


getCardio : List Wod -> Int
getCardio wods =
    getFactor cardio wods


getPower : List Wod -> Int
getPower wods =
    getFactor power wods


getEndurance : List Wod -> Int
getEndurance wods =
    getFactor endurance wods


normalize : Wod -> Int
normalize wod =
    case wod of
        ForTime _ borders value ->
            round
                (100.0
                    * (value - borders.worst)
                    / (borders.best - borders.worst)
                )

        PRInfo _ borders value ->
            round
                (100.0
                    * (toFloat <| value - borders.worst)
                    / (toFloat <| borders.best - borders.worst)
                )

        ForReps _ borders value ->
            round
                (100.0
                    * (toFloat <| value - borders.worst)
                    / (toFloat <| borders.best - borders.worst)
                )


endurance : Wod -> Float
endurance wod =
    case wod of
        ForTime props _ _ ->
            props.endurance

        PRInfo props _ _ ->
            props.endurance

        ForReps props _ _ ->
            props.endurance


power : Wod -> Float
power wod =
    case wod of
        ForTime props _ _ ->
            props.power

        PRInfo props _ _ ->
            props.power

        ForReps props _ _ ->
            props.power


cardio : Wod -> Float
cardio wod =
    case wod of
        ForTime props _ _ ->
            props.cardio

        PRInfo props _ _ ->
            props.cardio

        ForReps props _ _ ->
            props.cardio


getFactor : (Wod -> Float) -> List Wod -> Int
getFactor factor wods =
    let
        weightedSum =
            let
                addWeightedValue w sum =
                    sum + toFloat (normalize w) * (factor w)
            in
                foldl addWeightedValue 0 wods

        sumOfWeights =
            foldl (\w sum -> sum + factor w) 0.0 wods
    in
        round (weightedSum / sumOfWeights)
