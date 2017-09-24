module Main exposing (..)

import Html exposing (Html, text, div, img, input, ul, li, option)
import Html.Attributes exposing (src, type_, min, max, value)
import Html.Events exposing (onInput)
import Dict exposing (Dict, insert)
import String exposing (toInt)
import Result exposing (withDefault)


type alias Wod =
    { id : String, name : String, min : Int, max : Int }



---- MODEL ----


type alias Model =
    { wods : List Wod, values : Dict String Int }


init : ( Model, Cmd Msg )
init =
    ( { wods = [ Wod "frn" "Fran" 10 20, Wod "jl" "Julie" 5 15 ]
      , values = Dict.empty
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Slide String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Slide id v ->
            ( { model | values = insert id (withDefault 0 (toInt v)) (model.values) }, Cmd.none )

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
                    , text "anatol"
                    ]
            )
        |> ul []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ img [ src "/logo.svg" ] []
            , div [] [ text "Your Elm App is working!" ]
            ]
        , div []
            [ renderSliders model.wods ]
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
