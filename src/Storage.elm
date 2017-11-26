port module Storage exposing (..)


port getWods : (List ( String, String ) -> msg) -> Sub msg


port saveWods : List ( String, String ) -> Cmd msg


port signIn : () -> Cmd msg


port signedIn :
    ({ displayName : Maybe String, profilePic : Maybe String } -> msg)
    -> Sub msg
