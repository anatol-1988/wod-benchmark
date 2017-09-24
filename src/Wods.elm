module Wods exposing (Wod, wods, getCardio, getPower, getEndurance)

import List exposing (foldl)
import String exposing (toInt)
import Result exposing (withDefault)


type alias Wod =
    { id : String
    , name : String
    , min : Int
    , max : Int
    , value : String
    , cardio : Float
    , endurance : Float
    , power : Float
    }


wods : List Wod
wods =
    [ Wod "frn" "Fran" 0 100 "50" 0.3 0.8 0.3
    , Wod "mrh" "Murph" 0 100 "50" 0.9 0.2 0.1
    , Wod "sq" "Best Squat" 0 100 "50" 0.1 0.3 0.9
    ]


getCardio : List Wod -> Int
getCardio wods =
    getFactor (\w -> .cardio w) wods


getPower : List Wod -> Int
getPower wods =
    getFactor (\w -> .power w) wods


getEndurance : List Wod -> Int
getEndurance wods =
    getFactor (\w -> .endurance w) wods


getFactor : (Wod -> Float) -> List Wod -> Int
getFactor factor wods =
    let
        weightedSum =
            foldl
                (\w sum ->
                    sum
                        + toFloat (withDefault 0 <| toInt <| .value w)
                        * factor w
                )
                0
                wods

        sumOfWeights =
            foldl (\w sum -> sum + factor w) 0.0 wods
    in
        round (weightedSum / sumOfWeights)
