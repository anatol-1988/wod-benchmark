module Wods exposing (Wod, wods, getCardio)

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
    let
        weightedSum =
            foldl
                (\w sum ->
                    sum
                        + toFloat (withDefault 0 <| toInt <| .value w)
                        * (.cardio w)
                )
                0
                wods

        sumOfWeights =
            foldl (\w sum -> sum + (.cardio w)) 0.0 wods
    in
        round (weightedSum / sumOfWeights)
