module Wods
    exposing
        ( Wod
        , WodType(..)
        , Gender(Male, Female)
        , Units(Imperial, Metric)
        , kg
        , lb
        , Range
        , Limits
        , wods
        , getCardio
        , getPower
        , getEndurance
        , getTotal
        , normalize
        , unitToString
        , WodId
        , Indicators
        , updateIndicators
        )

import List exposing (foldl)
import Dict exposing (Dict)
import Time exposing (Time, second, minute)
import Maybe exposing (map)


type alias Indicators =
    { cardio : Maybe Int
    , endurance : Maybe Int
    , power : Maybe Int
    , total : Maybe Int
    }


type alias WodValues =
    Dict String String


type alias Limits a =
    { worst : a
    , best : a
    }


type alias Range a =
    { man : Limits a
    , woman : Limits a
    }


type Units
    = Imperial
    | Metric


type WeightUnit
    = Pound Int
    | Kilogram Int


unitToString : WeightUnit -> String
unitToString unit =
    case unit of
        Pound val ->
            toString val

        Kilogram val ->
            toString val


kg : Int -> WeightUnit
kg n =
    Kilogram n


lb : Int -> WeightUnit
lb n =
    Pound n


poundsToKilograms : Int -> Int
poundsToKilograms pounds =
    round (0.45359237 * toFloat pounds)


add : WeightUnit -> WeightUnit -> WeightUnit
add unit1 unit2 =
    case unit1 of
        Pound s1 ->
            case unit2 of
                Pound s2 ->
                    Pound (s1 + s2)

                Kilogram s2 ->
                    Kilogram ((poundsToKilograms s1) + s2)

        Kilogram s1 ->
            case unit2 of
                Pound s2 ->
                    Kilogram ((poundsToKilograms s2) + s1)

                Kilogram s2 ->
                    Kilogram (s1 + s2)


div : WeightUnit -> WeightUnit -> WeightUnit
div unit1 unit2 =
    case unit1 of
        Pound s1 ->
            case unit2 of
                Pound s2 ->
                    Pound (s1 + s2)

                Kilogram s2 ->
                    Kilogram ((poundsToKilograms s1) + s2)

        Kilogram s1 ->
            case unit2 of
                Pound s2 ->
                    Kilogram ((poundsToKilograms s2) + s1)

                Kilogram s2 ->
                    Kilogram (s1 + s2)


sub : WeightUnit -> WeightUnit -> WeightUnit
sub unit1 unit2 =
    case unit2 of
        Pound s2 ->
            add unit1 (lb -s2)

        Kilogram s2 ->
            add unit1 (kg -s2)


divide : WeightUnit -> WeightUnit -> Float
divide unit1 unit2 =
    case unit1 of
        Pound s1 ->
            case unit2 of
                Pound s2 ->
                    toFloat s1 / toFloat s2

                Kilogram s2 ->
                    toFloat (poundsToKilograms s1) / toFloat s2

        Kilogram s1 ->
            case unit2 of
                Pound s2 ->
                    toFloat (poundsToKilograms s2) / toFloat s1

                Kilogram s2 ->
                    toFloat s1 / toFloat s2


type WodType
    = ForTime (Range Time)
    | PRInfo (Range WeightUnit)
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
                }
      }
    , { id = "sq"
      , name = "Back Squat"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = kg 40, best = kg 215 }
                , woman = { worst = kg 30, best = kg 205 }
                }
      }
    , { id = "clj"
      , name = "Clean and Jerk"
      , cardio = 0.1
      , endurance = 0.4
      , power = 1.0
      , range =
            PRInfo
                { man = { worst = kg 20, best = kg 170 }
                , woman = { worst = kg 10, best = kg 160 }
                }
      }
    , { id = "sntch"
      , name = "Snatch"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = kg 15, best = kg 140 }
                , woman = { worst = kg 5, best = kg 120 }
                }
      }
    , { id = "dlft"
      , name = "Deadlift"
      , cardio = 0.1
      , endurance = 0.3
      , power = 0.9
      , range =
            PRInfo
                { man = { worst = kg 60, best = kg 260 }
                , woman = { worst = kg 50, best = kg 240 }
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
                }
      }
    ]


getCardio : WodValues -> Gender -> Units -> Maybe Int
getCardio values gender units =
    getFactor .cardio values gender units


getPower : WodValues -> Gender -> Units -> Maybe Int
getPower values gender units =
    getFactor .power values gender units


getEndurance : WodValues -> Gender -> Units -> Maybe Int
getEndurance values gender units =
    getFactor .endurance values gender units


getTotal : List (Maybe Int) -> Maybe Int
getTotal factors =
    foldl (\x y -> Maybe.map2 (*) x y) (Just 1) factors
        |> Maybe.map toFloat
        |> Maybe.map (flip (^) (1 / (List.length factors |> toFloat)))
        |> Maybe.map round


parseTime : String -> Maybe Time
parseTime str =
    let
        parts =
            String.split ":" str
    in
        case parts of
            m :: s :: _ ->
                Just <|
                    (Result.withDefault 0 <| String.toFloat m)
                        * minute
                        + (Result.withDefault 0 <| String.toFloat s)
                        * second

            s :: _ ->
                Result.toMaybe <| Result.map ((*) second) (String.toFloat s)

            _ ->
                Nothing


normalize : WodType -> Gender -> Units -> String -> Maybe Int
normalize wod gender units value =
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
                        (\x ->
                            (x - (genderLimits range).worst)
                                / ((genderLimits range).best
                                    - (genderLimits range).worst
                                  )
                        )
                    <|
                        parseTime value

                PRInfo range ->
                    Maybe.map
                        (\x ->
                            divide (sub x (genderLimits range).worst)
                                (sub (genderLimits range).best
                                    (genderLimits range).worst
                                )
                        )
                        (Maybe.map
                            (case units of
                                Imperial ->
                                    lb

                                Metric ->
                                    kg
                            )
                         <|
                            Result.toMaybe <|
                                String.toInt value
                        )

                ForReps range ->
                    Maybe.map
                        (\x ->
                            (toFloat <| x - (genderLimits range).worst)
                                / (toFloat <|
                                    (genderLimits range).best
                                        - (genderLimits range).worst
                                  )
                        )
                        (Result.toMaybe <| String.toInt value)
    in
        Maybe.map (\x -> round <| 100.0 * (clamp 0.0 1.0 x)) ratio


getFactor : (Wod -> Float) -> WodValues -> Gender -> Units -> Maybe Int
getFactor factor values gender units =
    let
        addWeightedValue w sum =
            sum
                + (toFloat <| Maybe.withDefault 0 <| normalizeValue w)
                * (factor w)

        normalizeValue wod =
            (Dict.get wod.id values)
                |> Maybe.andThen (normalize wod.range gender units)

        weightedSum =
            foldl addWeightedValue 0 wods

        sumOfWeights =
            foldl
                (\w sum ->
                    if (normalizeValue w) /= Nothing then
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


updateIndicators : WodValues -> Gender -> Units -> Indicators
updateIndicators values gender units =
    let
        cardio =
            getCardio values gender units

        endurance =
            getEndurance values gender units

        power =
            getPower values gender units
    in
        { cardio = cardio
        , endurance = endurance
        , power = power
        , total = getTotal [ cardio, endurance, power ]
        }
