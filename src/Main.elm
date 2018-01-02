module Main exposing (..)

import Html exposing (Html, text, div, img, ul, li, option, label, h3)
import Html exposing (h4, i, button, span, p, a)
import Html.Attributes exposing (src, type_, min, max, value, class, id, href)
import Html.Events exposing (onInput, onClick, onMouseDown)
import Json.Decode as Decode exposing (Value)
import List
import String exposing (toInt)
import Result
import Wods exposing (Wod, WodType(..), normalize, WodId)
import Time exposing (Time, minute, second)
import Diagram exposing (plotBenchmarks, Indicator)
import Profile exposing (Profile, AuthorizationState(..), Gender(..))
import Platform exposing (Task)
import Ports
import Dict exposing (Dict, empty)


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


timeToString : Time -> String
timeToString time =
    let
        minutes =
            truncate <| Time.inMinutes time

        seconds =
            truncate <| Time.inSeconds <| time - toFloat minutes * minute

        toStringLeadingZero n =
            (if n < 10 then
                "0"
             else
                ""
            )
                ++ toString n
    in
        toString minutes ++ ":" ++ toStringLeadingZero seconds



---- MODEL ----


type alias Indicators =
    { cardio : Maybe Int
    , endurance : Maybe Int
    , power : Maybe Int
    , total : Maybe Int
    }


type alias Model =
    { wods : List Wod
    , values : Dict String String
    , indicators : Indicators
    , indicators_ : Indicators
    , profile : AuthorizationState
    }


init : ( Model, Cmd Msg )
init =
    ( { wods = Wods.wods
      , values = Dict.empty
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
      , profile = NotAuthorized Undefinite
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OnChangeValue WodId String
    | OnChangeInterval WodId String
    | OnChangeGender Gender
    | CalcAll
    | GetWods (List ( String, String ))
    | SignIn
    | SignedIn (Maybe Profile)


setWodValue : String -> String -> Wod -> Wod
setWodValue id value wod =
    { wod
        | range =
            if wod.id == id then
                case wod.range of
                    ForTime range ->
                        ForTime { range | value = parseTime value }

                    ForReps range ->
                        ForReps
                            { range
                                | value =
                                    Result.toMaybe <| toInt value
                            }

                    PRInfo range ->
                        PRInfo
                            { range
                                | value =
                                    Result.toMaybe <| toInt value
                            }
            else
                wod.range
    }


getSerializedResult : Wod -> Maybe ( String, String )
getSerializedResult wod =
    let
        val =
            case wod.range of
                ForTime range ->
                    Maybe.map timeToString range.value

                ForReps range ->
                    Maybe.map toString range.value

                PRInfo range ->
                    Maybe.map toString range.value
    in
        Maybe.map (\v -> ( wod.id, v )) val


updateWods : Dict WodId String -> List Wod -> List Wod
updateWods values wods =
    List.map
        (\w ->
            setWodValue w.id
                (Maybe.withDefault "" <| Dict.get w.id values)
                w
        )
        wods


updateIndicators : List Wod -> Indicators
updateIndicators wods =
    { cardio = Wods.getCardio wods
    , endurance = Wods.getEndurance wods
    , power = Wods.getPower wods
    , total = Wods.getTotal wods
    }


maskTime : String -> String
maskTime time =
    String.filter
        (\char ->
            List.member char
                [ '0'
                , '1'
                , '2'
                , '3'
                , '4'
                , '5'
                , '6'
                , '7'
                , '8'
                , '9'
                , ':'
                ]
        )
        time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeValue id value ->
            { model | values = Dict.insert id value model.values } ! []

        OnChangeInterval id interval ->
            { model
                | values =
                    Dict.insert id (maskTime interval) model.values
            }
                ! []

        CalcAll ->
            let
                updatedWods =
                    updateWods model.values model.wods
            in
                { model
                    | wods = updatedWods
                    , indicators_ = model.indicators
                    , indicators = updateIndicators updatedWods
                }
                    ! [ case model.profile of
                            Authorized profile ->
                                ( profile.userUid
                                , List.filterMap getSerializedResult updatedWods
                                )
                                    |> Ports.saveWods

                            NotAuthorized _ ->
                                Cmd.none
                      ]

        GetWods wods ->
            let
                savedWods =
                    Dict.fromList wods

                updatedWods =
                    updateWods savedWods model.wods
            in
                { model
                    | values = savedWods
                    , indicators = updateIndicators updatedWods
                }
                    ! [ Ports.updateInputFields () ]

        SignIn ->
            model ! [ Ports.signIn () ]

        SignedIn profile ->
            (case profile of
                Just p ->
                    { model | profile = Authorized p }

                Nothing ->
                    model
            )
                ! []

        OnChangeGender gender ->
            { model
                | profile =
                    case model.profile of
                        Authorized profile ->
                            Authorized { profile | gender = gender }

                        NotAuthorized _ ->
                            NotAuthorized gender
            }
                ! []

        none ->
            model ! []



---- VIEW ----


renderInput : Maybe String -> Wod -> Html Msg
renderInput value wod =
    div [ class "col s12" ]
        [ (div [ Html.Attributes.class "input-field" ] <|
            (case wod.range of
                ForTime range ->
                    [ Html.input
                        [ type_ "text"
                        , Html.Attributes.id wod.id
                        , onInput (OnChangeInterval wod.id)
                        , Html.Attributes.class "validate"
                        , Maybe.withDefault "" value |> Html.Attributes.value
                        ]
                        []
                    ]

                ForReps range ->
                    [ Html.input
                        [ type_ "number"
                        , Html.Attributes.id wod.id
                        , Html.Attributes.min <| toString range.worst
                        , Html.Attributes.max <| toString range.best
                        , onInput (OnChangeValue wod.id)
                        , Maybe.withDefault "" value |> Html.Attributes.value
                        ]
                        []
                    ]

                PRInfo range ->
                    [ Html.input
                        [ type_ "number"
                        , Html.Attributes.id wod.id
                        , Html.Attributes.min <| toString range.worst
                        , Html.Attributes.max <| toString range.best
                        , onInput (OnChangeValue wod.id)
                        , Maybe.withDefault "" value |> Html.Attributes.value
                        ]
                        []
                    ]
            )
                ++ [ label
                        [ Html.Attributes.for wod.id
                        , Html.Attributes.class "active"
                        ]
                        [ text wod.name ]
                   , span [ class "unit" ]
                        [ text
                            (case wod.range of
                                ForTime range ->
                                    "mm:ss"

                                PRInfo range ->
                                    "kg"

                                ForReps range ->
                                    "reps"
                            )
                        ]
                   ]
          )
        ]


renderInputs : Model -> List (Html Msg)
renderInputs model =
    model.wods
        |> List.map
            (\w ->
                div [ Html.Attributes.class "row" ]
                    [ renderInput (Dict.get w.id model.values) w ]
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
                ++ renderInputs model
        , div [ class "col s12 m6" ]
            [ div [ class "row" ]
                [ h3 [] [ text "Your scores" ]
                , h4 [] [ text "For today" ]
                , plotBenchmarks { width = 480, height = 480 }
                    (getIndicator
                        "Fit Score"
                        model.indicators.total
                        model.indicators_.total
                    )
                    [ getIndicator "Cardio"
                        model.indicators.cardio
                        model.indicators_.cardio
                    , getIndicator "Endurance"
                        model.indicators.endurance
                        model.indicators_.endurance
                    , getIndicator "Power"
                        model.indicators.power
                        model.indicators_.power
                    ]
                ]
            ]
        , div [ class "col s12 m3" ]
            [ div [ class "row" ] <| viewProfile model.profile ]
        ]


getIndicator : String -> Maybe Int -> Maybe Int -> Indicator
getIndicator name1 value oldValue =
    { name = name1
    , score = Maybe.withDefault 0 value
    , diff = Maybe.map2 (-) value oldValue
    }


viewProfile : AuthorizationState -> List (Html Msg)
viewProfile state =
    let
        defaultMalePic =
            "https://i.imgur.com/icElw27.png"

        defaultFemalePic =
            "https://i.imgur.com/aoiCOAo.png"

        profilePic =
            case state of
                NotAuthorized gender ->
                    case gender of
                        Undefinite ->
                            defaultMalePic

                        Male ->
                            defaultMalePic

                        Female ->
                            defaultFemalePic

                Authorized profile ->
                    Maybe.withDefault defaultMalePic profile.profilePic

        displayName =
            case state of
                NotAuthorized _ ->
                    "Anonymous"

                Authorized profile ->
                    Maybe.withDefault "Anonymous" profile.displayName
    in
        [ div [ class "user-details grey lighten-3" ]
            [ div [ class "row valign-wrapper" ]
                [ div [ class "col s4 m4 l4 offset-l1" ]
                    [ img
                        [ class "circle responsive-img valign profile-image"
                        , Html.Attributes.src profilePic
                        ]
                        []
                    ]
                , div [ class "col s8 m8 l6" ]
                    [ span [ class "card-title" ]
                        [ text displayName ]
                    ]
                , case state of
                    Authorized _ ->
                        text ""

                    NotAuthorized _ ->
                        div [ class "card-action" ]
                            [ a
                                [ id "signin"
                                , onClick SignIn
                                , class "waves-effect waves-light btn"
                                ]
                                [ text "Sign In" ]
                            ]
                ]
            , div [ class "row" ]
                [ div [ class "col s4 m4 l4 offset-l1" ]
                    [ Html.form []
                        [ p []
                            [ Html.input
                                [ Html.Attributes.name "gender"
                                , type_ "radio"
                                , id "male"
                                , onClick (OnChangeGender Male)
                                ]
                                []
                            , Html.label [ Html.Attributes.for "male" ]
                                [ text "Male" ]
                            ]
                        , p []
                            [ Html.input
                                [ Html.Attributes.name "gender"
                                , type_ "radio"
                                , id "female"
                                , onClick (OnChangeGender Female)
                                ]
                                []
                            , Html.label [ Html.Attributes.for "female" ]
                                [ text "Female" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.getWods GetWods, Sub.map SignedIn sessionChange ]


sessionChange : Sub (Maybe Profile)
sessionChange =
    Ports.onSignedIn (Decode.decodeValue Profile.decode >> Result.toMaybe)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
