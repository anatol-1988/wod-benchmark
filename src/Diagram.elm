module Diagram exposing (plotBenchmarks, Size, Indicator)

import Svg exposing (Svg, circle, svg, text_, text, line, polyline, defs)
import Svg exposing (linearGradient, stop)
import Svg.Attributes exposing (cx, cy, x, y, r, viewBox, height, width, x1, x2)
import Svg.Attributes exposing (y1, y2, points, stopColor, fill, offset)
import Svg.Attributes exposing (class, id, alignmentBaseline, textAnchor)
import Html exposing (Html)


type alias Size =
    { width : Int
    , height : Int
    }


type alias Indicator =
    { name : String
    , score : Int
    , diff : Maybe Int
    }


type alias Polar =
    ( Float, Float )


type alias Cartesian =
    ( Int, Int )


plotToPolar : Cartesian -> Cartesian -> Polar
plotToPolar ( x, y ) ( centerX, centerY ) =
    let
        ( r, theta ) =
            toPolar ( toFloat (x - centerX), toFloat (y - centerY) )
    in
        ( r, theta + pi / 2 )


plotFromPolar : Polar -> Cartesian -> Cartesian
plotFromPolar ( r, theta ) ( centerX, centerY ) =
    let
        ( x, y ) =
            fromPolar ( r, theta - pi / 2 )
    in
        ( round x + centerX, round y + centerY )


drawAxes : Cartesian -> Int -> List (Svg msg)
drawAxes ( centerX, centerY ) num =
    let
        drawAxis angle =
            let
                ( x, y ) =
                    plotFromPolar ( 100, angle ) ( centerX, centerY )
            in
                line
                    [ y1 <| toString centerY
                    , y2 <| toString y
                    , x1 <| toString centerX
                    , x2 <| toString x
                    , class "diagram-axis"
                    ]
                    []

        angles =
            List.map (\n -> 2.0 * pi * toFloat n / toFloat num) <|
                List.range 0 (num - 1)
    in
        List.map drawAxis angles


zip =
    List.map2 (,)


drawTriangles : Cartesian -> List { a | score : Int } -> List (Svg msg)
drawTriangles ( centerX, centerY ) values =
    let
        drawTriangle n p1 p2 p3 =
            polyline
                [ points
                    ([ p1, p2, p3 ]
                        |> List.map
                            (\( x, y ) -> toString x ++ " " ++ toString y)
                        |> String.join ", "
                    )
                , class "diagram-triangle"
                , id <| "triangle" ++ (toString n)
                , fill <| "url(#grad" ++ (toString n) ++ ")"
                ]
                []

        coordinates =
            List.indexedMap
                (\n v ->
                    plotFromPolar
                        ( toFloat v.score
                        , (2.0 * pi * toFloat n / (toFloat <| List.length values))
                        )
                        ( centerX, centerY )
                )
                values

        circledCoordinates =
            coordinates
                ++ [ Maybe.withDefault ( 0, 0 ) (List.head coordinates) ]

        triangleVertexes =
            zip circledCoordinates <|
                Maybe.withDefault [] (List.tail circledCoordinates)
    in
        triangleVertexes
            |> List.indexedMap
                (\n ( p1, p2 ) -> drawTriangle n p1 ( centerX, centerY ) p2)


plotBenchmarks : Size -> Indicator -> List Indicator -> Html.Html msg
plotBenchmarks size total results =
    let
        numberOfResults =
            List.length results

        centerX =
            size.width // 2

        centerY =
            size.height // 2

        plotToPolar ( x, y ) =
            let
                ( r, theta ) =
                    toPolar ( toFloat (x - centerX), toFloat (y - centerY) )
            in
                ( r, theta + pi / 2 )

        plotFromPolar ( r, theta ) =
            let
                ( x, y ) =
                    fromPolar ( toFloat r, theta - pi / 2 )
            in
                ( round x + centerX, round y + centerY )

        drawCircle n res =
            drawResult
                ( 165, 2.0 * pi * toFloat n / toFloat numberOfResults )
                (toString n)
                res

        drawResult ( r, theta ) scoreId res =
            let
                ( centerX, centerY ) =
                    plotFromPolar ( r, theta )

                ( nameX, nameY ) =
                    ( centerX, centerY + 15 )

                ( diffX, diffY ) =
                    ( centerX + 23, centerY )

                ( diffText, diffClass ) =
                    case res.diff of
                        Just n ->
                            if n > 0 then
                                ( "+" ++ toString n, "positive" )
                            else if n < 0 then
                                ( "−" ++ (toString <| abs n), "negative" )
                            else
                                ( "", "zero" )

                        Nothing ->
                            ( "", "zero" )
            in
                [ text_
                    [ x (toString nameX)
                    , y (toString nameY)
                    , alignmentBaseline "hanging"
                    , textAnchor "middle"
                    , class "result-title"
                    ]
                    [ text res.name ]
                , text_
                    [ x (toString centerX)
                    , y (toString centerY)
                    , alignmentBaseline "middle"
                    , textAnchor "middle"
                    , class <| "result-score"
                    , id scoreId
                    ]
                    [ text <| toString res.score ]
                , text_
                    [ x (toString diffX)
                    , y (toString diffY)
                    , alignmentBaseline "middle"
                    , textAnchor "start"
                    , class <| "result-diff " ++ diffClass
                    ]
                    [ text diffText ]
                ]

        gradients =
            [ linearGradient [ id "grad0", x1 "0", x2 "0", y1 "0", y2 "1" ]
                [ stop [ offset "0%", stopColor "#9c66d0" ] []
                , stop [ offset "100%", stopColor "#ec5b70" ] []
                ]
            , linearGradient [ id "grad1", x1 "0", x2 "0", y1 "0", y2 "1" ]
                [ stop [ offset "0%", stopColor "#aa1c78" ] []
                , stop [ offset "100%", stopColor "#e31636" ] []
                ]
            , linearGradient [ id "grad2", x1 "0", x2 "0", y1 "0", y2 "1" ]
                [ stop [ offset "0%", stopColor "#8c42be" ] []
                , stop [ offset "100%", stopColor "#e53753" ] []
                ]
            ]
    in
        svg
            [ viewBox <|
                "0 0 "
                    ++ toString size.width
                    ++ " "
                    ++ toString size.height
            ]
        <|
            [ defs [] gradients ]
                ++ (List.concat <| List.indexedMap drawCircle results)
                ++ [ circle
                        [ cx (toString centerX)
                        , cy (toString centerY)
                        , r "60"
                        , id "central"
                        , class "diagram-circle"
                        ]
                        []
                   ]
                ++ drawResult ( 100, pi ) "totalScore" total
                ++ drawAxes ( centerX, centerY ) numberOfResults
                ++ drawTriangles ( centerX, centerY ) results
