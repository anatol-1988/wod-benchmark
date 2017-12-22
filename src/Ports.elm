port module Ports
    exposing
        ( getWods
        , saveWods
        , signIn
        , onSignedIn
        , updateInputFields
        )

import Json.Encode exposing (Value)


port getWods : (List ( String, String ) -> msg) -> Sub msg


port saveWods : ( String, List ( String, String ) ) -> Cmd msg


port signIn : () -> Cmd msg


port onSignedIn : (Value -> msg) -> Sub msg


port updateInputFields : () -> Cmd msg
