module Wods exposing (Wod, wods)


type alias Wod =
    { id : String, name : String, min : Int, max : Int, value : String }


wods : List Wod
wods =
    [ Wod "frn" "Fran" 0 100 "50", Wod "jl" "Julie" 0 100 "50" ]
