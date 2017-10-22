module Diagram exposing (plotBenchmarks, Size)

import Svg exposing (circle, svg, text_, text)
import Svg.Attributes exposing (cx, cy, x, y, r, viewBox, height, width)
import Svg.Attributes exposing (class, id, alignmentBaseline, textAnchor)
import Html exposing (Html)


type alias Size =
    { width : Int
    , height : Int
    }


type alias Result =
    { name : String
    , score : Int
    , diff : Maybe Int
    }


plotBenchmarks : Size -> Result -> List Result -> Html.Html msg
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
            let
                ( x, y ) =
                    plotFromPolar
                        ( 30
                        , 2.0 * pi * toFloat n / toFloat numberOfResults
                        )
            in
                [ circle
                    [ cx (toString x)
                    , cy (toString y)
                    , r "80"
                    , id <| "circle" ++ (toString n)
                    , class "diagram-circle"
                    ]
                    []
                ]
                    ++ (drawResult
                            ( 165
                            , 2.0 * pi * toFloat n / toFloat numberOfResults
                            )
                            (toString n)
                            res
                       )

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
                                ( toString 0, "zero" )

                        Nothing ->
                            ( toString 0, "zero" )
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
    in
        svg
            [ viewBox <|
                "0 0 "
                    ++ toString size.width
                    ++ " "
                    ++ toString size.height
            ]
            ((List.concat <| List.indexedMap drawCircle results)
                ++ [ circle
                        [ cx (toString centerX)
                        , cy (toString centerY)
                        , r "60"
                        , id "central"
                        , class "diagram-circle"
                        ]
                        []
                   ]
                ++ drawResult ( 0, 0 ) "totalScore" total
            )
