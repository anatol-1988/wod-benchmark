port module Storage exposing (..)

import Wods exposing (Wod)

port getWods : (List (String, String) -> msg) -> Sub msg
