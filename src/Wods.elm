module Wods exposing (Wod, wods, getCardio, getPower, getEndurance)

import List exposing (foldl)
import String exposing (toInt)
import Result exposing (withDefault)
import Time exposing (Time)


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
    [ ForReps
        { id = "frn"
        , name = "Fran"
        , cardio = 0.3
        , endurance = 0.8
        , power = 0.3
        }
        { worst = 0, best = 100 }
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
    , ForReps
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
    getFactor .cardio wods


getPower : List Wod -> Int
getPower wods =
    getFactor .power wods


getEndurance : List Wod -> Int
getEndurance wods =
    getFactor .endurance wods


getFactor : (Wod -> Float) -> List Wod -> Int
getFactor factor wods =
    let
        weightedSum =
            foldl
                (\w sum -> sum + toFloat (w.value) * (factor w))
                0
                wods

        sumOfWeights =
            foldl (\w sum -> sum + factor w) 0.0 wods
    in
        round (weightedSum / sumOfWeights)
