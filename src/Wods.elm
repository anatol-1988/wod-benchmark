module Wods exposing (Wod, wods, getCardio, getPower, getEndurance)

import List exposing (foldl)
import String exposing (toInt)
import Result exposing (withDefault)
import Time exposing (Time)


type alias Wod =
    { id : String
    , name : String
    , cardio : Float
    , endurance : Float
    , power : Float
    , wodType : WodType
    , min : Int
    , max : Int
    , value : Int
    }


type WodType
    = ForTime
    | PRInfo
    | ForReps


wods : List Wod
wods =
    [ Wod "frn" "Fran" 0.3 0.8 0.3 ForReps 0 100 50
    , Wod "mrh" "Murph" 0.9 0.2 0.1 ForReps 0 100 50
    , Wod "sq" "Best Squat" 0.1 0.3 0.9 ForReps 0 100 50
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
