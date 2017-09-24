module Main exposing (..)

import Html exposing (Html, text, div, img, input, ul, li, option)
import Html.Attributes exposing (src, type_, min, max, value, class)
import Html.Events exposing (onInput)
import List exposing (map)
import Wods exposing (Wod)


---- MODEL ----


type alias Model =
    { wods : List Wod }


init : ( Model, Cmd Msg )
init =
    ( { wods = Wods.wods }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Slide String String


setWodValue : String -> String -> Wod -> Wod
setWodValue id value wod =
    { wod
        | value =
            if wod.id == id then
                value
            else
                wod.value
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide id v ->
            ( { model | wods = map (setWodValue id v) model.wods }, Cmd.none )

        none ->
            ( model, Cmd.none )



---- VIEW ----


renderSliders : List Wod -> Html Msg
renderSliders wods =
    wods
        |> List.map
            (\w ->
                li []
                    [ text <| .name w
                    , input
                        [ type_ "range"
                        , Html.Attributes.min <| toString <| .min w
                        , Html.Attributes.max <| toString <| .max w
                        , onInput (Slide <| .id w)
                        ]
                        []
                    , text <| .value w
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
