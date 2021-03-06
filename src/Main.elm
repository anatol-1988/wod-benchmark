module Main exposing (..)

import Html exposing (Html, text, div, img, ul, li, option, label, h3)
import Html exposing (h4, i, button, span, p, a)
import Svg
import Svg.Attributes
import Html.Attributes exposing (src, type_, min, max, value, class, id, href)
import Html.Attributes exposing (checked)
import Html.Events exposing (onInput, onClick, onMouseDown)
import Json.Decode as Decode exposing (Value)
import List
import String exposing (toInt)
import Result
import Wods exposing (Wod, WodType(..), normalize, WodId, kg, lb, unitToString)
import Wods exposing (Indicators)
import Time exposing (Time, minute, second)
import Diagram exposing (plotBenchmarks, Indicator)
import Profile exposing (Profile, AuthorizationState(..), Gender(..), Units(..))
import Platform exposing (Task)
import Ports
import Dict exposing (Dict, empty)


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


type alias Model =
    { values : Dict String String
    , indicators : Indicators
    , indicators_ : Indicators
    , profile : AuthorizationState
    , gender : Gender
    , units : Units
    }


init : ( Model, Cmd Msg )
init =
    ( { values = Dict.empty
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
      , profile = NotAuthorized
      , gender = Undefinite
      , units = UndefiniteUnits
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | OnChangeValue WodId String
    | OnChangeInterval WodId String
    | OnChangeGender Gender
    | OnChangeUnits Units
    | CalcAll
    | GetWods (List ( String, String ))
    | SignIn
    | SignedIn (Maybe Profile)


wodGender : Profile.Gender -> Wods.Gender
wodGender profile =
    case profile of
        Male ->
            Wods.Male

        Female ->
            Wods.Female

        Undefinite ->
            Wods.Male


wodUnits : Profile.Units -> Wods.Units
wodUnits units =
    case units of
        UndefiniteUnits ->
            Wods.Imperial

        Metric ->
            Wods.Metric

        Imperial ->
            Wods.Imperial


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


onEnter : msg -> Html.Attribute msg
onEnter msg =
    Html.Events.keyCode
        |> Decode.andThen
            (\key ->
                if key == 13 then
                    Decode.succeed msg
                else
                    Decode.fail "Not enter"
            )
        |> Html.Events.on "keyup"


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
            { model
                | indicators_ = model.indicators
                , indicators =
                    Wods.updateIndicators model.values
                        (wodGender model.gender)
                        (wodUnits model.units)
            }
                ! [ case model.profile of
                        Authorized profile ->
                            ( profile.userUid
                            , Dict.toList model.values
                            )
                                |> Ports.saveWods

                        NotAuthorized ->
                            Cmd.none
                  ]

        GetWods wods ->
            let
                savedWods =
                    Dict.fromList wods
            in
                { model
                    | values = savedWods
                    , indicators =
                        Wods.updateIndicators model.values
                            (wodGender model.gender)
                            (wodUnits model.units)
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
                | gender = gender
                , indicators =
                    Wods.updateIndicators model.values
                        (wodGender gender)
                        (wodUnits model.units)
            }
                ! [ case model.profile of
                        Authorized profile ->
                            ( profile.userUid, toString gender )
                                |> Ports.saveGender

                        NotAuthorized ->
                            Cmd.none
                  ]

        OnChangeUnits units ->
            { model
                | units = units
                , indicators =
                    Wods.updateIndicators model.values
                        (wodGender model.gender)
                        (wodUnits model.units)
            }
                ! [ case model.profile of
                        Authorized profile ->
                            ( profile.userUid, toString units )
                                |> Ports.saveUnits

                        NotAuthorized ->
                            Cmd.none
                  ]

        none ->
            model ! []



---- VIEW ----


genderLimits : Wods.Range a -> Profile.Gender -> Wods.Limits a
genderLimits range gender =
    case wodGender gender of
        Wods.Male ->
            range.man

        Wods.Female ->
            range.woman


renderInput : Maybe String -> Wod -> Profile.Gender -> Units -> Html Msg
renderInput value wod gender units =
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
                        , Html.Attributes.min <|
                            toString (genderLimits range gender).worst
                        , Html.Attributes.max <|
                            toString (genderLimits range gender).best
                        , onInput (OnChangeValue wod.id)
                        , Maybe.withDefault "" value |> Html.Attributes.value
                        ]
                        []
                    ]

                PRInfo range ->
                    [ Html.input
                        [ type_ "number"
                        , Html.Attributes.id wod.id
                        , Html.Attributes.min <|
                            toString (genderLimits range gender).worst
                        , Html.Attributes.max <|
                            toString (genderLimits range gender).best
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
                                    case units of
                                        UndefiniteUnits ->
                                            "lb"

                                        Imperial ->
                                            "lb"

                                        Metric ->
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
    Wods.wods
        |> List.map
            (\w ->
                div [ Html.Attributes.class "row" ]
                    [ renderInput (Dict.get w.id model.values)
                        w
                        model.gender
                        model.units
                    ]
            )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col s12 m3", onEnter CalcAll ] <|
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
            [ div [ class "row" ] <|
                viewProfile model.profile model.gender model.units
            ]
        ]


getIndicator : String -> Maybe Int -> Maybe Int -> Indicator
getIndicator name1 value oldValue =
    { name = name1
    , score = Maybe.withDefault 0 value
    , diff = Maybe.map2 (-) value oldValue
    }


iconSvg : String -> Html Msg
iconSvg name =
    Html.span [ class "button_icon" ]
        [ Svg.svg [ Svg.Attributes.class "icon" ]
            [ Svg.use
                [ Svg.Attributes.xlinkHref <| "/sprite.svg#" ++ name ]
                []
            ]
        ]


viewProfile : AuthorizationState -> Gender -> Units -> List (Html Msg)
viewProfile state gender units =
    let
        defaultMalePic =
            "https://i.imgur.com/icElw27.png"

        defaultFemalePic =
            "https://i.imgur.com/aoiCOAo.png"

        profilePic =
            case state of
                NotAuthorized ->
                    case gender of
                        Female ->
                            defaultFemalePic

                        _ ->
                            defaultMalePic

                Authorized profile ->
                    Maybe.withDefault defaultMalePic profile.profilePic

        displayName =
            case state of
                NotAuthorized ->
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
                , div [ class "col s8 m8 l8" ]
                    [ span [ class "card-title" ]
                        [ text displayName ]
                    ]
                ]
            , div [ class "row" ]
                [ case state of
                    Authorized _ ->
                        text ""

                    NotAuthorized ->
                        a
                            [ id "signin"
                            , onClick SignIn
                            , class
                                "waves-effect waves-light btn social facebook"
                            ]
                            [ iconSvg "fb", text "Sign In" ]
                ]
            , div [ class "row" ]
                [ div [ class "col s4 m4 l4 offset-l1" ]
                    [ Html.form []
                        [ p []
                            [ Html.input
                                [ Html.Attributes.name "gender"
                                , type_ "radio"
                                , id "male"
                                , checked (gender == Male)
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
                                , checked (gender == Female)
                                , onClick (OnChangeGender Female)
                                ]
                                []
                            , Html.label [ Html.Attributes.for "female" ]
                                [ text "Female" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col s4 m4 l4 offset-l1" ]
                    [ Html.form []
                        [ p []
                            [ Html.input
                                [ Html.Attributes.name "units"
                                , type_ "radio"
                                , id "imperial"
                                , checked (units == Imperial)
                                , onClick (OnChangeUnits Imperial)
                                ]
                                []
                            , Html.label [ Html.Attributes.for "imperial" ]
                                [ text "Imperial" ]
                            ]
                        , p []
                            [ Html.input
                                [ Html.Attributes.name "units"
                                , type_ "radio"
                                , id "metric"
                                , checked (units == Metric)
                                , onClick (OnChangeUnits Metric)
                                ]
                                []
                            , Html.label [ Html.Attributes.for "metric" ]
                                [ text "Metric" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.getWods GetWods
        , Sub.map SignedIn <|
            Ports.onSignedIn
                (Decode.decodeValue Profile.decodeProfile >> Result.toMaybe)
        , Sub.map OnChangeUnits <| Ports.onUnitsChanged Profile.stringToUnits
        , Sub.map OnChangeGender <| Ports.onGenderChanged Profile.stringToGender
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
