module SimpleGraph exposing (Point, GraphAttributes, Option(..)
   , DataWindow, lineChart, lineChartAsSVG
   , barChart, lineChartWithDataWindow, lineChartAsSVGWithDataWindow, barChartAsSVG
   , scatterPlot, scatterPlotAsSVG)

{-| **SimpleGraph** is a bare-bones package for rendering data as
line and bar charts, both as HTML and as SVG.

For examples, see the REAdME.
For a demo, see <https://jxxcarlson.github.io/app/gamblers_ruin.html>


## Making a chart

@docs lineChart, lineChartWithDataWindow, barChart, scatterPlot


## Types and Options

@docs Point, DataWindow, GraphAttributes, Option


## SVG

@docs lineChartAsSVG, lineChartAsSVGWithDataWindow, barChartAsSVG, scatterPlotAsSVG

-}

import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, circle, svg, text, text_)
import Svg.Attributes as SA


{-| A GraphAttributes value defines the size on
the screen occupied by the graph and the color of the
line.
-}
type alias GraphAttributes =
    { graphHeight : Float
    , graphWidth : Float
    , options : List Option
    }


{-| Use the options field to customize the line chart.
Examples: (1) [Color "blue"] makes the charted line
blue (2) [Color "blue", XTickmarks 10, YTickmarks 5]
places 10 tick marks along the x-axis and 5
tick marks along the y-axis, (3) options = [ ]
produces a bare-bones graph.

The DeltaX option is used to specify the distnce
from the leading edge of one bar to the next
in bar graph.

The Scale option rescales the graph along both the
x and y axes. Negative parameters flip the graph.
Thus `Scale 1.0 -1.0` flips the graph in the y direction.

An important use of the Scale option is to correct
the misbehavor of Safari, which presents the graphs upside down (!!)

-}
type Option
    = Color String
    | XTickmarks Int
    | YTickmarks Int
    | DeltaX Float
    | Scale Float Float


{-| A DataWindow is a rectangle which determines
the x and y ranges of the data to be displayed..
-}
type alias DataWindow =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    }


{-| The data to be graphed by SimpleGraph.asHtml is
a List Point.
-}
type alias Point =
    ( Float, Float )


type alias Segment =
    ( Point, Point )


type alias ScaleFactor =
    { kx : Float
    , ky : Float
    }


{-| Render a list of points (pairs of floats)
to Html as a line chart. using the parameters
of GraphAttributes. GraphAttributes controls
the appearance of the graph -- width, height, color,
tick marks on the axes. The x-coordinates of the
data are assumed to be in increasing order.
-}
lineChart : GraphAttributes -> List Point -> Html msg
lineChart ga data =
    lineChartWithDataWindow (getDataWindow data) ga data


{-| Render a list of points to SVG as a line chart using the parameters
of GraphAttributes.
-}
lineChartAsSVG : GraphAttributes -> List Point -> Svg msg
lineChartAsSVG ga data =
    lineChartAsSVGWithDataWindow (getDataWindow data) ga data


{-| This function is like lineChart, but with the additional
DataWindow parameter. A DataWindow defines the range of
x and y coordinates that are displayed. In lineChart, the
DataWindow is deduced from the data presented.
-}
lineChartWithDataWindow : DataWindow -> GraphAttributes -> List Point -> Html msg
lineChartWithDataWindow dw ga data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 50)
        , SA.viewBox <| "-40 -20 " ++ String.fromFloat (ga.graphWidth + 50) ++ " " ++ String.fromFloat (ga.graphHeight + 40)
        ]
        [ lineChartAsSVGWithDataWindow dw ga data ]


{-| Render a list of points to Svg as a line chart using the parameters
of GraphAttributes and DataWindow.
-}
lineChartAsSVGWithDataWindow : DataWindow -> GraphAttributes -> List Point -> Svg msg
lineChartAsSVGWithDataWindow dw ga data =
    let
        scaleFactor =
            getScaleFactor dw ga

        render : List Point -> Svg msg
        render data_ =
            data_
                |> translate ( -dw.xMin, -dw.yMin )
                |> rescale scaleFactor
                |> segments
                |> segmentsToSVG ga.options

        renderPlain : List Point -> Svg msg
        renderPlain data_ =
            data_
                |> translate ( -dw.xMin, -dw.yMin )
                |> rescale scaleFactor
                |> segments
                |> segmentsToSVG []

        theData =
            data |> render

        abscissa =
            [ ( dw.xMin, 0 ), ( dw.xMax, 0 ) ] |> renderPlain

        ordinate =
            [ ( dw.xMin, dw.yMin ), ( dw.xMin, dw.yMax ) ] |> renderPlain

        boundingBox_ : Svg msg
        boundingBox_ =
            boundingBox ga.options dw |> renderPlain

        xTickMarks_ =
            makeXTickMarks scaleFactor renderPlain dw (xTickmarks ga.options)

        yTickMarks_ =
            makeYTickMarks scaleFactor renderPlain dw (yTickmarks ga.options)

        xLabels =
            makeXLabels scaleFactor dw (xTickmarks ga.options)

        yLabels =
            makeYLabels scaleFactor dw (yTickmarks ga.options)

        transformer =
            SA.transform (buildSVGTransformString ga)
    in
        g [ transformer ] [ theData, abscissa, ordinate, boundingBox_, xTickMarks_, yTickMarks_, xLabels, yLabels ]


buildSVGTransformString : GraphAttributes -> String
buildSVGTransformString ga =
    let
        ( kx, ky ) =
            scale ga.options

        scaleString =
            "scale(" ++ (String.fromFloat kx) ++ ", " ++ String.fromFloat ky ++ ")"

        translateY =
            case ky < 0 of
                False ->
                    "0"

                True ->
                    String.fromFloat -ga.graphHeight

        translateX =
            case kx < 0 of
                False ->
                    "0"

                True ->
                    String.fromFloat (-ga.graphWidth + 60)

        translateString =
            "translate(" ++ translateX ++ ", " ++ translateY ++ ")"
    in
        scaleString ++ " " ++ translateString



--
-- BARCHART
--


{-| Render a list of numbers to Html as a bar chart using the parameters
of GraphAttributes.
-}
barChart : GraphAttributes -> List Float -> Html msg
barChart ga data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 40)
        , SA.viewBox <| "-60 -20 " ++ String.fromFloat (ga.graphWidth + 40) ++ " " ++ String.fromFloat (ga.graphHeight + 20)
        ]
        [ barChartAsSVG ga data ]


{-| Render a list of numbers to Svg as a bar chart using the parameters
of GraphAttributes.
-}
barChartAsSVG : GraphAttributes -> List Float -> Svg msg
barChartAsSVG ga data =
    let
        barWidth =
            0.8 * (deltaX ga.options)

        gbar : (Float, Float) -> Svg msg
        gbar =
            \( x, y ) -> barRect (lineColor ga.options) barWidth ga.graphHeight x y

        ordinate =
            segmentToSVG [] ( ( 0, 0 ), ( 0, ga.graphHeight ) )

        abscissa : Svg msg
        abscissa =
            segmentToSVG [] ( ( 0, 0 ), ( ga.graphWidth, 0 ) )

        xTickmarks2 =
            bxTickmarks ga

        yTickmarks2 =
            byTickmarks ga

        ( yMax, preparedData ) =
            prepare (deltaX ga.options) data

        yLabels =
            bMakeYLabels yMax ga

        transformer : Svg.Attribute msg
        transformer =
            SA.transform (buildSVGTransformString ga)
    in
        List.map gbar preparedData
            ++ [ abscissa, ordinate, xTickmarks2, yTickmarks2, yLabels ]
            |> g [ transformer ]



{-| Make a scatter plot of a list points, render as Html
-}
scatterPlot : GraphAttributes -> List Point -> Html msg
scatterPlot ga data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 50)
        , SA.viewBox <| "0 0" ++ String.fromFloat (ga.graphWidth + 50) ++ " " ++ String.fromFloat (ga.graphHeight + 50)
        ]
        [scatterPlotAsSVG  ga data]


{-| Make a scatter plot of a list points, render as SVG
-}
scatterPlotAsSVG : GraphAttributes -> List Point -> Svg msg
scatterPlotAsSVG ga data =
    let
        dw = getDataWindow data



        xScaleFactor = ga.graphWidth/(dw.xMax - dw.xMin)
        yScaleFactor = ga.graphHeight/(dw.yMax - dw.yMin)


        diameter =
            4.0
         -- dot color diameter x y
        dot_ : (Float, Float) -> Svg msg
        dot_ =
            \( x, y ) -> dot (lineColor ga.options) diameter x y

        scaleFactor =
            getScaleFactor dw ga

        renderPlain : List Point -> Svg msg
        renderPlain data_ =
            data_
                |> translate ( -dw.xMin, -dw.yMin )
                |> rescale scaleFactor
                |> segments
                |> segmentsToSVG []

        boundingBox_ : Svg msg
        boundingBox_ =
                boundingBox ga.options {dw | xMax = dw.xMax + diameter/xScaleFactor, yMax = dw.yMax + diameter/yScaleFactor} |> renderPlain

        --xTickmarks2 =
        --    bxTickmarks ga
        --
        --yTickmarks2 =
        --    byTickmarks ga
        --
        --yLabels =
        --    bMakeYLabels dw.yMax ga

        transformer : Svg.Attribute msg
        transformer =
            SA.transform (buildSVGTransformString ga)

        rescaledData = List.map (\(x,y) -> (xScaleFactor*x, yScaleFactor*y)) data

        rendered : List (Svg msg)
        rendered =   List.map dot_ rescaledData
    in
        g [ transformer ]  (rendered ++ [boundingBox_])






{- Below are functions used to render line and bar charts but which are not exported -}
--
-- TRANSFORMATIONS
--


rescale : ( Float, Float ) -> List Point -> List Point
rescale ( kx, ky ) data =
    List.map (\( x, y ) -> ( kx * x, ky * y )) data


translate : ( Float, Float ) -> List Point -> List Point
translate ( dx, dy ) data =
    List.map (\( x, y ) -> ( x + dx, y + dy )) data


boundingBox : List Option -> DataWindow -> List Point
boundingBox options dw =
    case ( xTickmarks options, yTickmarks options ) of
        ( 0, 0 ) ->
            []

        ( _, _ ) ->
            [ ( dw.xMin, dw.yMin ), ( dw.xMax, dw.yMin ), ( dw.xMax, dw.yMax ), ( dw.xMin, dw.yMax ), ( dw.xMin, dw.yMin ) ]


{-| Create a DataWindow from a list of points. This will
be the smallest rectangle containing the data.
-}
getDataWindow : List Point -> DataWindow
getDataWindow pointList =
    let
        xs =
            List.map Tuple.first pointList

        ys =
            List.map Tuple.second pointList

        xMin =
            List.minimum xs |> Maybe.withDefault 0

        xMax =
            List.maximum xs |> Maybe.withDefault 0

        yMin =
            List.minimum ys |> Maybe.withDefault 0

        yMax =
            List.maximum ys |> Maybe.withDefault 0
    in
        { xMin = xMin
        , xMax = xMax
        , yMin = yMin
        , yMax = yMax
        }


getScaleFactor : DataWindow -> GraphAttributes -> ( Float, Float )
getScaleFactor dataWindow gA =
    let
        kx =
            gA.graphWidth / (dataWindow.xMax - dataWindow.xMin)

        ky =
            gA.graphHeight / (dataWindow.yMax - dataWindow.yMin)
    in
        ( kx, ky )



--
-- MANIPULATING AND RENDERING SEGMENTS
--


segments : List a -> List ( a, a )
segments list =
    let
        n =
            List.length list
    in
        List.map2 Tuple.pair (List.take (n - 1) list) (List.drop 1 list)


segmentToSVG : List Option -> Segment -> Svg msg
segmentToSVG options ( ( x1, y1 ), ( x2, y2 ) ) =
    line
        [ SA.x1 (String.fromFloat x1)
        , SA.y1 (String.fromFloat y1)
        , SA.x2 (String.fromFloat x2)
        , SA.y2 (String.fromFloat y2)
        , SA.stroke <| lineColor options
        , SA.strokeWidth "1"
        ]
        []


segmentsToSVG : List Option -> List Segment -> Svg msg
segmentsToSVG options segmentList =
    segmentList |> List.map (segmentToSVG options) |> (\x -> g [] x)



--
-- OPTION HANDLING
--


deltaX : List Option -> Float
deltaX options =
    findMap deltaX_ options |> Maybe.withDefault 15


deltaX_ : Option -> Maybe Float
deltaX_ option =
    case option of
        DeltaX dx ->
            Just dx

        _ ->
            Nothing


lineColor : List Option -> String
lineColor options =
    findMap lineColor_ options |> Maybe.withDefault "rgb(40, 40, 40)"


lineColor_ : Option -> Maybe String
lineColor_ option =
    case option of
        Color str ->
            Just str

        _ ->
            Nothing


xTickmarks : List Option -> Int
xTickmarks options =
    findMap xTickmarks_ options |> Maybe.withDefault 0


xTickmarks_ : Option -> Maybe Int
xTickmarks_ option =
    case option of
        XTickmarks k ->
            Just k

        _ ->
            Nothing


yTickmarks : List Option -> Int
yTickmarks options =
    findMap yTickmarks_ options |> Maybe.withDefault 0


yTickmarks_ : Option -> Maybe Int
yTickmarks_ option =
    case option of
        YTickmarks k ->
            Just k

        _ ->
            Nothing


scale : List Option -> ( Float, Float )
scale options =
    findMap scale_ options |> Maybe.withDefault ( 1.0, 1.0 )


scale_ : Option -> Maybe ( Float, Float )
scale_ option =
    case option of
        Scale kx ky ->
            Just ( kx, ky )

        _ ->
            Nothing



--
-- LABELS FOR LINE CHART
--


makeYLabel : ( Float, Float ) -> DataWindow -> Float -> Svg msg
makeYLabel ( kx, ky ) dw y =
    let
        dy =
            String.fromFloat (ky * (y - dw.yMin) - 3)

        label =
            String.fromFloat <| roundTo 1 y
    in
        text_
            [ SA.transform <| "translate(0," ++ dy ++ ") scale(1,-1)"
            , SA.x <| String.fromFloat -30
            , SA.y <| "0"
            , SA.fontSize "9px"
            ]
            [ text label ]


makeXLabels : ( Float, Float ) -> DataWindow -> Int -> Svg msg
makeXLabels ( kx, ky ) dw n =
    case n == 0 of
        True ->
            g [] []

        False ->
            List.range 0 (n - 1)
                |> List.map (\k -> dw.xMin + (toFloat k) * (dw.xMax - dw.xMin) / (toFloat (n - 1)))
                |> List.map (makeXLabel ( kx, ky ) dw)
                |> (\x -> g [] x)


makeXLabel : ( Float, Float ) -> DataWindow -> Float -> Svg msg
makeXLabel ( kx, ky ) dw x =
    let
        dx =
            String.fromFloat (kx * (x - dw.xMin))

        label =
            String.fromFloat <| roundTo 1 x
    in
        text_
            [ SA.transform <| "translate(" ++ dx ++ ",0) scale(1,-1)"
            , SA.x <| "-8"
            , SA.y <| "20"
            , SA.fontSize "9px"
            ]
            [ text label ]


makeYLabels : ( Float, Float ) -> DataWindow -> Int -> Svg msg
makeYLabels ( kx, ky ) dw n =
    case n == 0 of
        True ->
            g [] []

        False ->
            List.range 0 (n - 1)
                |> List.map (\k -> dw.yMin + (toFloat k) * (dw.yMax - dw.yMin) / (toFloat (n - 1)))
                |> List.map (makeYLabel ( kx, ky ) dw)
                |> (\x -> g [] x)



--
-- TICK MARKS FOR LINE CHART
--


tickMarkLength =
    7.5


makeYTickMark : Float -> DataWindow -> Float -> List Point
makeYTickMark kx dw y =
    [ ( dw.xMin, y ), ( dw.xMin - (tickMarkLength / kx), y ) ]


makeYTickMarks : ( Float, Float ) -> (List Point -> Svg msg) -> DataWindow -> Int -> Svg msg
makeYTickMarks ( kx, ky ) render dw n =
    case n == 0 of
        True ->
            [] |> List.map render |> (\x -> g [] x)

        False ->
            List.range 0 (n - 1)
                |> List.map (\k -> (toFloat k) * (dw.yMax - dw.yMin) / (toFloat (n - 1)))
                |> List.map (makeYTickMark kx dw)
                |> List.map (translate ( 0, dw.yMin ))
                |> List.map render
                |> (\x -> g [] x)


makeXTickMark : Float -> DataWindow -> Float -> List Point
makeXTickMark ky dw x =
    [ ( x, dw.yMin ), ( x, dw.yMin - (tickMarkLength / ky) ) ]


makeXTickMarks : ( Float, Float ) -> (List Point -> Svg msg) -> DataWindow -> Int -> Svg msg
makeXTickMarks ( kx, ky ) render dw n =
    case n == 0 of
        True ->
            [] |> List.map render |> (\x -> g [] x)

        False ->
            List.range 0 (n - 1)
                |> List.map (\k -> (toFloat k) * (dw.xMax - dw.xMin) / (toFloat (n - 1)))
                |> List.map (makeXTickMark (ky) dw)
                |> List.map (translate ( dw.xMin, 0 ))
                |> List.map render
                |> (\x -> g [] x)



--
-- PREPARE DATA FOR BAR CHARTS
--


prepare : Float -> List Float -> ( Float, List ( Float, Float ) )
prepare dx data =
    let
        xs =
            xCoordinates (List.length data) dx

        ymax =
            List.maximum data |> Maybe.withDefault 1

        ys =
            List.map (\y -> y / ymax) data
    in
        ( ymax, List.map2 Tuple.pair xs ys )


xCoordinates : Int -> Float -> List Float
xCoordinates n dx =
    List.map (\i -> toFloat i * dx) (List.range 0 n)



--
-- RENDER A BAR
--


barRect : String -> Float -> Float -> Float -> Float -> Svg msg
barRect color barWidth barHeight x fraction =
    rect
        [ SA.width <| String.fromFloat barWidth
        , SA.height <| String.fromFloat <| fraction * barHeight
        , SA.x <| String.fromFloat x
        , SA.fill color
        ]
        []

dot : String -> Float -> Float -> Float  -> Svg msg
dot color diameter x y  =
    rect
        [ SA.width <| String.fromFloat diameter
        , SA.height <| String.fromFloat diameter
        , SA.x <| String.fromFloat x
        , SA.y <|  String.fromFloat y
        , SA.fill color
        ]
        []

--
-- TICMARKS FOR BAR GRAPH
--


byTickmark : Float -> Svg msg
byTickmark y =
    segmentToSVG [] ( ( 0, y ), ( -8, y ) )


byTickmarks : GraphAttributes -> Svg msg
byTickmarks ga =
    let
        n =
            yTickmarks ga.options
    in
        List.range 0 (n - 1)
            |> List.map (\k -> (toFloat k) * ga.graphHeight / (toFloat (n - 1)))
            |> List.map byTickmark
            |> g []


bxTickmark : Float -> Svg msg
bxTickmark x =
    segmentToSVG [] ( ( x, 0 ), ( x, -8 ) )


bxTickmarks : GraphAttributes -> Svg msg
bxTickmarks ga =
    let
        dx =
            (toFloat <| xTickmarks ga.options) * (deltaX ga.options)

        n =
            round <| ga.graphWidth / dx
    in
        List.range 0 (n - 1)
            |> List.map (\k -> (toFloat k) * dx)
            |> List.map bxTickmark
            |> g []



--
-- LABELS FOR BAR GRAPH
--


bMakeYLabel : ( Float, Float ) -> Float -> Svg msg
bMakeYLabel ( yMax, graphHeight ) y =
    let
        label =
            String.fromFloat <| roundTo 1 y
    in
        text_
            [ SA.transform <| "translate(0," ++ "-3" ++ ") scale(1,-1)"
            , SA.x <| "-30"
            , SA.y <| String.fromFloat (-y * graphHeight / yMax)
            , SA.fontSize "9px"
            ]
            [ text label ]


bMakeYLabels : Float -> GraphAttributes -> Svg msg
bMakeYLabels yMax ga =
    let
        n =
            yTickmarks ga.options
    in
        case n == 0 of
            True ->
                g [] []

            False ->
                List.range 0 (n - 1)
                    |> List.map (\k -> (toFloat k) * yMax / (toFloat (n - 1)))
                    |> List.map (bMakeYLabel ( yMax, ga.graphHeight ))
                    |> g []



--
-- UTILITY
--


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap f list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case f x of
                Just v ->
                    Just v

                Nothing ->
                    findMap f xs


roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
        x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)
