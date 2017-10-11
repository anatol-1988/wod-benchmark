module Diagram exposing (plotBenchmarks, Size)

import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, r, viewBox, height, width, class, id)
import Html exposing (Html)


type alias Size =
    { width : Int
    , height : Int
    }


plotBenchmarks : Size -> Html.Html msg
plotBenchmarks size =
    let
        centerX =
            size.width // 2

        centerY =
            size.height // 2

        plotToPolar ( x, y ) =
            toPolar <| ( toFloat (x - centerX), toFloat (y - centerY) )

        plotFromPolar ( r, theta ) =
            let
                ( x, y ) =
                    fromPolar ( (toFloat r), theta )
            in
                ( round x + centerX, round y + centerY )

        drawCircle n =
            let
                ( x, y ) =
                    plotFromPolar ( 50, 2.0 * pi * (toFloat n) / 4 )
            in
                circle
                    [ cx (toString x)
                    , cy (toString y)
                    , r "80"
                    , id <| "circle" ++ (toString n)
                    , class "diagram-circle"
                    ]
                    []
    in
        svg
            [ width (toString size.width)
            , height (toString size.height)
            , viewBox <|
                "0 0 "
                    ++ toString size.width
                    ++ " "
                    ++ toString size.height
            ]
            -- circle [ cx (toString centerX), cy (toString centerY), r "60" ] []
            (List.map drawCircle (List.range 1 4))
