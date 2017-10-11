module Diagram exposing (plotBenchmarks, Size)

import Svg exposing (circle, svg, text_, text)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , x
        , y
        , r
        , viewBox
        , height
        , width
        , class
        , id
        , alignmentBaseline
        , textAnchor
        )
import Html exposing (Html)


type alias Size =
    { width : Int
    , height : Int
    }


type alias Result =
    { name : String
    , score : Int
    , diff : Int
    }


plotBenchmarks : Size -> Result -> Html.Html msg
plotBenchmarks size result =
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
                    fromPolar ( toFloat r, theta )
            in
                ( round x + centerX, round y + centerY )

        drawCircle n =
            let
                ( x, y ) =
                    plotFromPolar ( 30, 2.0 * pi * (toFloat n + 0.5) / 4 )
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
                    ++ (drawResult ( 140, 2.0 * pi * (toFloat n + 0.5) / 4 ) <|
                            toString n
                       )

        drawResult ( r, theta ) scoreId =
            let
                ( centerX, centerY ) =
                    plotFromPolar ( r, theta )

                ( nameX, nameY ) =
                    plotFromPolar ( r + 15, theta + pi / 2 )

                ( scoreX, scoreY ) =
                    plotFromPolar ( r, theta )

                ( diffX, diffY ) =
                    plotFromPolar ( r + 25, theta )

                ( diffText, diffClass ) =
                    if result.diff > 0 then
                        ( "+" ++ toString result.diff, "positive" )
                    else if result.diff < 0 then
                        ( "−" ++ toString result.diff, "negative" )
                    else
                        ( toString result.diff, "zero" )
            in
                [ text_
                    [ x (toString nameX)
                    , y (toString nameY)
                    , alignmentBaseline "hanging"
                    , textAnchor "middle"
                    , class "result-title"
                    ]
                    [ text result.name ]
                , text_
                    [ x (toString scoreX)
                    , y (toString scoreY)
                    , alignmentBaseline "middle"
                    , textAnchor "middle"
                    , class <| "result-score"
                    , id scoreId
                    ]
                    [ text <| toString result.score ]
                , text_
                    [ x (toString diffX)
                    , y (toString diffY)
                    , alignmentBaseline "middle"
                    , textAnchor "start"
                    , class <| "result-diff " ++ diffClass
                    ]
                    [ text <| diffText ]
                ]
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
            (List.concatMap drawCircle (List.range 1 4)
                ++ [ circle
                        [ cx (toString centerX)
                        , cy (toString centerY)
                        , r "60"
                        , id <| "central"
                        , class "diagram-circle"
                        ]
                        []
                   ]
                ++ drawResult ( 0, 0 ) "totalScore"
            )
