module Main exposing (..)

import Html exposing (Html, text, div, img, input, ul, li, option)
import Html.Attributes exposing (src, type_, min, max, value, class)
import Html.Events exposing (onInput)
import List exposing (map)
import String exposing (toInt)
import Result exposing (withDefault)
import Wods exposing (Wod(..), normalize)
import Date.Extra.Create exposing (timeFromFields)
import Date exposing (toTime, fromTime)
import Time exposing (Time)


parseTime : String -> Time
parseTime str =
    let
        parts =
            String.split ":" str

        date =
            case parts of
                m :: s :: _ ->
                    timeFromFields 0
                        (withDefault 0 <| toInt m)
                        (withDefault 0 <| toInt s)
                        0

                _ ->
                    timeFromFields 0 0 0 0
    in
        toTime date



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
    case wod of
        ForTime props borders val ->
            if props.id == id then
                ForTime props borders <| parseTime value
            else
                ForTime props borders val

        ForReps props borders val ->
            if props.id == id then
                ForReps props borders <| withDefault 0 <| toInt value
            else
                ForReps props borders val

        PRInfo props borders val ->
            if props.id == id then
                PRInfo props borders <| withDefault 0 <| toInt value
            else
                PRInfo props borders val


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide id v ->
            let
                value = v
            in
                ( { model | wods = map (setWodValue id value) model.wods }
                , Cmd.none
                )

        none ->
            ( model, Cmd.none )



---- VIEW ----


renderSliders : List Wod -> Html Msg
renderSliders wods =
    wods
        |> List.map
            (\w ->
                case w of
                    ForTime props range v ->
                        li []
                            [ text <| props.name
                            , input
                                [ type_ "text"
                                , onInput (Slide props.id)
                                ]
                                []
                            , text <| toString <| Wods.normalize w
                            ]

                    ForReps props range v ->
                        li []
                            [ text <| props.name
                            , input
                                [ type_ "number"
                                , Html.Attributes.min <| toString <| range.worst
                                , Html.Attributes.max <| toString <| range.best
                                , Html.Attributes.placeholder <| "reps"
                                , onInput (Slide props.id)
                                ]
                                []
                            , text <| toString <| Wods.normalize w
                            ]

                    PRInfo props range v ->
                        li []
                            [ text <| props.name
                            , input
                                [ type_ "number"
                                , Html.Attributes.min <| toString <| range.worst
                                , Html.Attributes.max <| toString <| range.best
                                , Html.Attributes.placeholder <| "kg"
                                ]
                                []
                            , text <| toString <| Wods.normalize w
                            ]
            )
        |> ul []


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col s6" ]
            [ renderSliders model.wods ]
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
