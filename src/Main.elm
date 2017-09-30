module Main exposing (..)

import Html exposing (Html, text, div, img, input, ul, li, option)
import Html.Attributes exposing (src, type_, min, max, value, class)
import Html.Events exposing (onInput)
import List exposing (map)
import String exposing (toInt)
import Result exposing (withDefault)
import Wods exposing (Wod, WodType(..), normalize)
import Date.Extra.Create exposing (timeFromFields)
import Date exposing (toTime, fromTime)
import Time exposing (Time)
import Plot exposing (..)


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
    { wods : List Wod }


init : ( Model, Cmd Msg )
init =
    ( { wods = Wods.wods }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Slide String String


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

        none ->
            ( model, Cmd.none )



---- VIEW ----


renderInputs : List Wod -> Html Msg
renderInputs wods =
    wods
        |> List.map
            (\w ->
                li []
                    [ text <| w.name
                    , case w.range of
                        ForTime range ->
                            input
                                [ type_ "text"
                                , Html.Attributes.placeholder <| "mm:ss"
                                , onInput (Slide w.id)
                                ]
                                []

                        ForReps range ->
                            input
                                [ type_ "number"
                                , Html.Attributes.min <| toString <| range.worst
                                , Html.Attributes.max <| toString <| range.best
                                , Html.Attributes.placeholder <| "reps"
                                , onInput (Slide w.id)
                                ]
                                []

                        PRInfo range ->
                            input
                                [ type_ "number"
                                , Html.Attributes.min <| toString <| range.worst
                                , Html.Attributes.max <| toString <| range.best
                                , Html.Attributes.placeholder <| "kg"
                                , onInput (Slide w.id)
                                ]
                                []
                    , text <|
                        "Normalized: "
                            ++ (toString <| Wods.normalize w.range)
                    ]
            )
        |> ul []


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col s6" ]
            [ renderInputs model.wods ]
        , div [ class "col s6" ]
            [ div [ class "row" ]
                [ text <|
                    "Cardio: "
                        ++ (toString <| Wods.getCardio model.wods)
                ]
            , div [ class "row" ]
                [ text <|
                    "Endurance: "
                        ++ (toString <| Wods.getEndurance model.wods)
                ]
            , div [ class "row" ]
                [ text <| "Power: " ++ (toString <| Wods.getPower model.wods) ]
            , div [ class "row" ]
                [ viewBars
                    (groups (List.map (\data -> group data.label data.heights)))
                    [ { label = "Cardio"
                      , heights = [ toFloat <| Wods.getCardio model.wods ]
                      }
                    , { label = "Endurance"
                      , heights = [ toFloat <| Wods.getEndurance model.wods ]
                      }
                    , { label = "Power"
                      , heights = [ toFloat <| Wods.getPower model.wods ]
                      }
                    ]
                ]
            ]
        , div [ class "row" ]
            [ text <|
                "General estimation: "
                    ++ (toString <| Wods.getTotalEstimation model.wods)
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
