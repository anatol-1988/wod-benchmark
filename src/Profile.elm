module Profile
    exposing
        ( Profile
        , AuthorizationState(..)
        , Gender(..)
        , decode
        , stringToGender
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


stringToGender : String -> Gender
stringToGender str =
    case str of
        "male" ->
            Male

        "female" ->
            Female

        _ ->
            Undefinite


decodeGender : Decoder Gender
decodeGender =
    Decode.string |> Decode.andThen (Decode.succeed << stringToGender)


decode : Decoder Profile
decode =
    Pipeline.decode Profile
        |> required "displayName" (Decode.nullable Decode.string)
        |> required "profilePic" (Decode.nullable Decode.string)
        |> required "identifier" Decode.string
        |> required "userUid" Decode.string
