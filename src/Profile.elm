module Profile
    exposing
        ( Profile
        , AuthorizationState(..)
        , Gender(..)
        , decode
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)


type alias Profile =
    { displayName : Maybe String
    , profilePic : Maybe String
    , identifier : String
    , userUid : String
    , gender : Gender
    }


type AuthorizationState
    = NotAuthorized Gender
    | Authorized Profile


type Gender
    = Undefinite
    | Male
    | Female


decodeGender : Decoder Gender
decodeGender =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "undefinite" ->
                        Decode.succeed Undefinite

                    "male" ->
                        Decode.succeed Male

                    "female" ->
                        Decode.succeed Female

                    str ->
                        Decode.fail <| "Unknown gender " ++ str
            )


decode : Decoder Profile
decode =
    Pipeline.decode Profile
        |> required "displayName" (Decode.nullable Decode.string)
        |> required "profilePic" (Decode.nullable Decode.string)
        |> required "identifier" Decode.string
        |> required "userUid" Decode.string
        |> required "gender" decodeGender
