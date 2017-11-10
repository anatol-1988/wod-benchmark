port module Storage exposing (..)

port getWods : (List (String, String) -> msg) -> Sub msg

port setWods : List (String, String) -> Cmd msg
