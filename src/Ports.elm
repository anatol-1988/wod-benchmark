port module Ports exposing (..)


port getWods : (List ( String, String ) -> msg) -> Sub msg


port saveWods : ( String, List ( String, String ) ) -> Cmd msg


port signIn : () -> Cmd msg


port signedIn :
    ({ displayName : Maybe String
     , profilePic : Maybe String
     , identifier : String
     , userUid : String
     }
     -> msg
    )
    -> Sub msg


port updateInputFields : () -> Cmd msg
