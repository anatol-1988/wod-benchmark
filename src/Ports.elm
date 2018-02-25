port module Ports
    exposing
        ( getWods
        , saveWods
        , saveGender
        , saveUnits
        , signIn
        , onSignedIn
        , updateInputFields
        , onGenderChanged
        , onUnitsChanged
        )

import Json.Encode exposing (Value)


port getWods : (List ( String, String ) -> msg) -> Sub msg


port saveWods : ( String, List ( String, String ) ) -> Cmd msg


port saveUnits : ( String, String ) -> Cmd msg


port saveGender : ( String, String ) -> Cmd msg


port signIn : () -> Cmd msg


port onSignedIn : (Value -> msg) -> Sub msg


port onGenderChanged : (String -> msg) -> Sub msg


port onUnitsChanged : (String -> msg) -> Sub msg


port updateInputFields : () -> Cmd msg
