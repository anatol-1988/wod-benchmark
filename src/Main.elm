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


type alias Model =
    { wods : List Wod
    , indicators :
        { cardio : Maybe Int
        , endurance : Maybe Int
        , power : Maybe Int
        , total : Maybe Int
        }
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
        Slide id v ->
            let
                value =
                    v
            in
                ( { model | wods = map (setWodValue id value) model.wods }
                , Cmd.none
                )

        CalcAll ->
            ( { model
                | indicators =
                    { cardio = Wods.getCardio model.wods
                    , endurance = Wods.getEndurance model.wods
                    , power = Wods.getPower model.wods
                    , total = Wods.getTotal model.wods
                    }
              }
            , Cmd.none
            )

        none ->
            ( model, Cmd.none )



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
            , label [ Html.Attributes.class "unit" ]
                [ text <| "mm:ss" ]
            ]

        ForReps range ->
            [ input
                [ type_ "number"
                , Html.Attributes.id wod.id
                , Html.Attributes.min <| toString range.worst
                , Html.Attributes.max <| toString range.best
                , onInput (Slide wod.id)
                , Html.Attributes.defaultValue <| toString range.best
                ]
                []
            , label [ Html.Attributes.class "unit" ]
                [ text <| "reps" ]
            ]

        PRInfo range ->
            [ input
                [ type_ "number"
                , Html.Attributes.id wod.id
                , Html.Attributes.min <| toString range.worst
                , Html.Attributes.max <| toString range.best
                , onInput (Slide wod.id)
                , Html.Attributes.defaultValue <| toString range.best
                ]
                []
            , label [ Html.Attributes.class "unit" ]
                [ text <| "kg" ]
            ]
    )
        ++ [ label
                [ Html.Attributes.for wod.id
                , Html.Attributes.class "active"
                ]
                [ text <| wod.name ]
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
        , div [ class "col s12 m5" ]
            [ div [ class "row" ]
                [ h3 [] [ text "Your scores" ]
                , h4 [] [ text "For today" ]
                , plotBenchmarks { width = 480, height = 480 }
                    { name = "Fit Score"
                    , score =
                        Maybe.withDefault 0 model.indicators.total
                    , diff = 5
                    }
                    [ { name = "Cardio"
                      , score =
                            Maybe.withDefault 0 model.indicators.cardio
                      , diff = 5
                      }
                    , { name = "Endurance"
                      , score =
                            Maybe.withDefault 0 model.indicators.endurance
                      , diff = -5
                      }
                    , { name = "Power"
                      , score =
                            Maybe.withDefault 0 model.indicators.power
                      , diff = -15
                      }
                    ]
                ]
            ]
        , div [ class "col s12 m4" ]
            [ div [ class "card blue-grey darken-1" ]
                [ div [ class "card-content white-text" ]
                    [ span [ class "card-title" ] [ text "Card Title" ]
                    , p [] [ text "I am a very simple card. I am good at containing small bits of information.\n              I am convenient because I require little markup to use effectively." ]
                    ]
                , div [ class "card-action" ]
                    [ a [ href "#" ] [ text "This is a link" ]
                    , a [ href "#" ] [ text "This is a link" ]
                    ]
                ]
            , div [ class "card blue-grey darken-1" ]
                [ div [ class "card-content white-text" ]
                    [ span [ class "card-title" ] [ text "Card Title" ]
                    , p [] [ text "I am a very simple card. I am good at containing small bits of information.\n              I am convenient because I require little markup to use effectively." ]
                    ]
                , div [ class "card-action" ]
                    [ a [ href "#" ] [ text "This is a link" ]
                    , a [ href "#" ] [ text "This is a link" ]
                    ]
                ]
            , div [ class "card blue-grey darken-1" ]
                [ div [ class "card-content white-text" ]
                    [ span [ class "card-title" ] [ text "Card Title" ]
                    , p [] [ text "I am a very simple card. I am good at containing small bits of information.\n              I am convenient because I require little markup to use effectively." ]
                    ]
                , div [ class "card-action" ]
                    [ a [ href "#" ] [ text "This is a link" ]
                    , a [ href "#" ] [ text "This is a link" ]
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
