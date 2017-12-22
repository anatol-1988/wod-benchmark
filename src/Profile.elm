module Profile
    exposing
        ( Profile
        , AuthorizationState(..)
        , Gender(..)
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


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

decoderGender : Decoder Gender
decoderGender toDecode =
    let
        value = Decode.decodeString Decode.string toDecode
    in
        case value of
            Ok name -> 
                case name of
                    "undefinite" -> Undefinite
                    "male" -> Male
                    "female" -> Female
            Err _ -> Undefinite

decoder : Decoder Profile
decoder =
    decode Profile
        |> required "displayName" (Decode.nullable Decode.string)
        |> required "profilePic" (Decode.nullable Decode.string)
        |> required "identifier" Decode.string
        |> required "userId" Decode.string
        |> required "gender" decoderGender
