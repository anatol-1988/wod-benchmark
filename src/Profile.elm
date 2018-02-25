module Profile
    exposing
        ( Profile
        , AuthorizationState(..)
        , Gender(..)
        , Units(..)
        , decodeProfile
        , stringToGender
        , stringToUnits
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)


type alias Profile =
    { displayName : Maybe String
    , profilePic : Maybe String
    , identifier : String
    , userUid : String
    }


type AuthorizationState
    = NotAuthorized
    | Authorized Profile


type Gender
    = Undefinite
    | Male
    | Female


type Units
    = UndefiniteUnits
    | Imperial
    | Metric


stringToGender : String -> Gender
stringToGender str =
    case str of
        "Male" ->
            Male

        "Female" ->
            Female

        _ ->
            Undefinite


decodeGender : Decoder Gender
decodeGender =
    Decode.string |> Decode.andThen (Decode.succeed << stringToGender)


stringToUnits : String -> Units
stringToUnits str =
    case str of
        "Imperial" ->
            Imperial

        "Metric" ->
            Metric

        _ ->
            UndefiniteUnits


decodeUnits : Decoder Units
decodeUnits =
    Decode.string |> Decode.andThen (Decode.succeed << stringToUnits)


decodeProfile : Decoder Profile
decodeProfile =
    Pipeline.decode Profile
        |> required "displayName" (Decode.nullable Decode.string)
        |> required "profilePic" (Decode.nullable Decode.string)
        |> required "identifier" Decode.string
        |> required "userUid" Decode.string
