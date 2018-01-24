module Wods
    exposing
        ( Wod
        , WodType(..)
        , Gender(Male, Female)
        , Range
        , Limits
        , wods
        , getCardio
        , getPower
        , getEndurance
        , getTotal
        , normalize
        , WodId
        )

import List exposing (foldl)
import Time exposing (Time, second, minute)
import Arithmetic exposing (cubeRoot)


type alias Limits a =
    { worst : a
    , best : a
    }


type alias Range a =
    { man : Limits a
    , woman : Limits a
    , value : Maybe a
    }


type WodType
    = ForTime (Range Time)
    | PRInfo (Range Int)
    | ForReps (Range Int)


type Gender
    = Male
    | Female


type alias WodId =
    String


type alias Wod =
    { id : WodId
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
                { man =
                    { worst = 5 * minute
                    , best = 2 * minute + 1 * second
                    }
                , woman =
                    { worst = 6 * minute
                    , best = 2 * minute + 30 * second
                    }
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
                { man =
                    { worst = 60 * minute
                    , best = 30 * minute
                    }
                , woman =
                    { worst = 70 * minute
                    , best = 40 * minute
                    }
                , value = Nothing
                }
      }
    , { id = "sq"
      , name = "Back Squat"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = 40, best = 215 }
                , woman = { worst = 30, best = 205 }
                , value = Nothing
                }
      }
    , { id = "clj"
      , name = "Clean and Jerk"
      , cardio = 0.1
      , endurance = 0.4
      , power = 1.0
      , range =
            PRInfo
                { man = { worst = 20, best = 170 }
                , woman = { worst = 10, best = 160 }
                , value = Nothing
                }
      }
    , { id = "sntch"
      , name = "Snatch"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = 15, best = 140 }
                , woman = { worst = 5, best = 120 }
                , value = Nothing
                }
      }
    , { id = "dlft"
      , name = "Deadlift"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = 60, best = 260 }
                , woman = { worst = 50, best = 240 }
                , value = Nothing
                }
      }
    , { id = "fgb"
      , name = "Fight Gone Bad"
      , cardio = 0.9
      , endurance = 0.6
      , power = 0.3
      , range =
            ForReps
                { man = { worst = 200, best = 508 }
                , woman = { worst = 190, best = 490 }
                , value = Nothing
                }
      }
    , { id = "plps"
      , name = "Max Pull-ups"
      , cardio = 0.9
      , endurance = 0.6
      , power = 0.3
      , range =
            ForReps
                { man = { worst = 5, best = 75 }
                , woman = { worst = 2, best = 65 }
                , value = Nothing
                }
      }
    , { id = "grc"
      , name = "Grace"
      , cardio = 0.3
      , endurance = 0.9
      , power = 0.7
      , range =
            ForTime
                { man =
                    { worst = 6 * minute
                    , best = 1 * minute + 11 * second
                    }
                , woman =
                    { worst = 7 * minute
                    , best = 2 * minute + 11 * second
                    }
                , value = Nothing
                }
      }
    ]


getCardio : List Wod -> Gender -> Maybe Int
getCardio wods gender =
    getFactor .cardio gender wods


getPower : List Wod -> Gender -> Maybe Int
getPower wods gender =
    getFactor .power gender wods


getEndurance : List Wod -> Gender -> Maybe Int
getEndurance wods gender =
    getFactor .endurance gender wods


getTotal : List Wod -> Gender -> Maybe Int
getTotal wods gender =
    let
        getRoot cardio power endurance =
            round <| cubeRoot (toFloat <| cardio * power * endurance)
    in
        Maybe.map3 getRoot (getCardio wods gender) (getPower wods gender) (getEndurance wods gender)


normalize : WodType -> Gender -> Maybe Int
normalize wod gender =
    let
        genderLimits range =
            case gender of
                Male ->
                    range.man

                Female ->
                    range.woman

        ratio =
            case wod of
                ForTime range ->
                    Maybe.map
                        (\x -> (x - (genderLimits range).worst) / ((genderLimits range).best - (genderLimits range).worst))
                        range.value

                PRInfo range ->
                    Maybe.map
                        (\x ->
                            (toFloat <| x - (genderLimits range).worst)
                                / (toFloat <| (genderLimits range).best - (genderLimits range).worst)
                        )
                        range.value

                ForReps range ->
                    Maybe.map
                        (\x ->
                            (toFloat <| x - (genderLimits range).worst)
                                / (toFloat <| (genderLimits range).best - (genderLimits range).worst)
                        )
                        range.value
    in
        Maybe.map (\x -> round <| 100.0 * (clamp 0.0 1.0 x)) ratio


getFactor : (Wod -> Float) -> Gender -> List Wod -> Maybe Int
getFactor factor gender wods =
    let
        weightedSum =
            let
                addWeightedValue w sum =
                    sum
                        + (toFloat <|
                            Maybe.withDefault 0 <|
                                normalize w.range gender
                          )
                        * (factor w)
            in
                foldl addWeightedValue 0 wods

        sumOfWeights =
            foldl
                (\w sum ->
                    if (normalize w.range gender) /= Nothing then
                        sum + factor w
                    else
                        sum
                )
                0.0
                wods
    in
        if sumOfWeights > 0.0 then
            Just <| round (weightedSum / sumOfWeights)
        else
            Nothing
