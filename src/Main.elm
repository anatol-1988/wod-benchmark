module Main exposing (..)

import Html exposing (Html, text, div, img, input, ul, li, option, label, h3, h4)
import Html exposing (i, button, span, p, a)
import Html.Attributes exposing (src, type_, min, max, value, class, id, href)
import Html.Events exposing (onInput, onClick, onMouseDown)
import List exposing (map)
import String exposing (toInt)
import Result exposing (withDefault)
import Wods exposing (Wod, WodType(..), normalize)
import Date.Extra.Create exposing (timeFromFields)
import Date exposing (toTime, fromTime)
import Time exposing (Time)
import Diagram exposing (plotBenchmarks)
import Markdown exposing (toHtml)
import Platform exposing (Task)


parseTime : String -> Maybe Time
parseTime str =
    let
        parts =
            String.split ":" str
    in
        case parts of
            m :: s :: _ ->
                Just <|
                    toTime <|
                        timeFromFields 0
                            (withDefault 0 <| toInt m)
                            (withDefault 0 <| toInt s)
                            0

            _ ->
                Nothing



---- MODEL ----


type alias Indicators =
    { cardio : Maybe Int
    , endurance : Maybe Int
    , power : Maybe Int
    , total : Maybe Int
    }


type alias Model =
    { wods : List Wod
    , indicators : Indicators
    , indicators_ : Indicators
    }


init : ( Model, Cmd Msg )
init =
    ( { wods = Wods.wods
      , indicators =
            { cardio = Nothing
            , endurance = Nothing
            , power = Nothing
            , total = Nothing
            }
      , indicators_ =
            { cardio = Nothing
            , endurance = Nothing
            , power = Nothing
            , total = Nothing
            }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Slide String String
    | CalcAll


setWodValue : String -> String -> Wod -> Wod
setWodValue id value wod =
    case wod.range of
        ForTime range ->
            if wod.id == id then
                { wod | range = ForTime { range | value = parseTime value } }
            else
                { wod | range = ForTime { range | value = range.value } }

        ForReps range ->
            if wod.id == id then
                { wod
                    | range =
                        ForReps
                            { range | value = Result.toMaybe <| toInt value }
                }
            else
                { wod | range = ForReps { range | value = range.value } }

        PRInfo range ->
            if wod.id == id then
                { wod
                    | range =
                        PRInfo
                            { range | value = Result.toMaybe <| toInt value }
                }
            else
                { wod | range = PRInfo { range | value = range.value } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide id value ->
            { model | wods = map (setWodValue id value) model.wods }
                ! []

        CalcAll ->
            { model
                | indicators_ = model.indicators
                , indicators =
                    { cardio = Wods.getCardio model.wods
                    , endurance = Wods.getEndurance model.wods
                    , power = Wods.getPower model.wods
                    , total = Wods.getTotal model.wods
                    }
            }
                ! []

        none ->
            model ! []



---- VIEW ----


renderInput : Wod -> List (Html Msg)
renderInput wod =
    (case wod.range of
        ForTime range ->
            [ input
                [ type_ "text"
                , Html.Attributes.id wod.id
                , onInput (Slide wod.id)
                , Html.Attributes.class "validate"
                ]
                []
            , span [ Html.Attributes.class "unit" ]
                [ text "mm:ss" ]
            ]

        ForReps range ->
            [ input
                [ type_ "number"
                , Html.Attributes.id wod.id
                , Html.Attributes.min <| toString range.worst
                , Html.Attributes.max <| toString range.best
                , onInput (Slide wod.id)
                ]
                []
            , span [ Html.Attributes.class "unit" ]
                [ text "reps" ]
            ]

        PRInfo range ->
            [ input
                [ type_ "number"
                , Html.Attributes.id wod.id
                , Html.Attributes.min <| toString range.worst
                , Html.Attributes.max <| toString range.best
                , onInput (Slide wod.id)
                ]
                []
            , span [ Html.Attributes.class "unit" ]
                [ text "kg" ]
            ]
    )
        ++ [ label
                [ Html.Attributes.for wod.id
                , Html.Attributes.class "active"
                ]
                [ text wod.name ]
           ]


renderInputs : List Wod -> List (Html Msg)
renderInputs wods =
    wods
        |> List.map
            (\w ->
                div [ Html.Attributes.class "row" ]
                    [ div [ Html.Attributes.class "input-field" ] <|
                        renderInput w
                    ]
            )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col s12 m3" ] <|
            [ h3 [] [ text "Calculated on previous benchmarks" ]
            , button
                [ class "waves-effect waves-light btn-large"
                , id "update"
                , onClick CalcAll
                ]
                [ i [ class "material-icons right" ]
                    [ text "cached" ]
                , text "Update"
                ]
            ]
                ++ (renderInputs model.wods)
        , div [ class "col s12 m6" ]
            [ div [ class "row" ]
                [ h3 [] [ text "Your scores" ]
                , h4 [] [ text "For today" ]
                , plotBenchmarks { width = 480, height = 480 }
                    { name = "Fit Score"
                    , score =
                        Maybe.withDefault 0 model.indicators.total
                    , diff =
                        Maybe.map2 (-)
                            model.indicators.total
                            model.indicators_.total
                    }
                    [ { name = "Cardio"
                      , score =
                            Maybe.withDefault 0 model.indicators.cardio
                      , diff =
                            Maybe.map2 (-)
                                model.indicators.cardio
                                model.indicators_.cardio
                      }
                    , { name = "Endurance"
                      , score =
                            Maybe.withDefault 0 model.indicators.endurance
                      , diff =
                            Maybe.map2 (-)
                                model.indicators.endurance
                                model.indicators_.endurance
                      }
                    , { name = "Power"
                      , score =
                            Maybe.withDefault 0 model.indicators.power
                      , diff =
                            Maybe.map2 (-)
                                model.indicators.power
                                model.indicators_.power
                      }
                    ]
                ]
            ]
        , div [ class "col s12 m3" ]
            [ div [ class "row" ]
                [ div [ class "card blue darken-4" ]
                    [ div [ class "card-content white-text" ]
                        [ toHtml []
                            """Your FitScore is 74 and you improved what is OK.
                               **We know how to move you further**"""
                        ]
                    , div [ class "card-action" ]
                        [ a [ href "#" ] [ text "Learn More" ] ]
                    ]
                , div [ class "card grey lighten-5" ]
                    [ div [ class "card-content" ]
                        [ toHtml []
                            """**41 points for weightlifting** means you have
                            to do more here. We recommend you to focus on
                            Cleans & Squats technique, then increase weight"""
                        ]
                    , div [ class "card-action" ]
                        [ a [ href "#" ] [ text "Got it" ] ]
                    ]
                , div [ class "card grey lighten-5" ]
                    [ div [ class "card-content" ]
                        [ toHtml []
                            """**You're doing great,** but you have at least
                            **2 areas** need your attention"""
                        ]
                    , div [ class "card-action" ]
                        [ a [ href "#" ] [ text "Got it" ] ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
