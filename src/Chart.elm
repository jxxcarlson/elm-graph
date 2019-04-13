module Chart exposing(Point, GraphAttributes, Option(..), DataWindow,  lineChart, lineChartAsSVG, barChart, lineChartWithDataWindow, lineChartAsSVGWithDataWindow, barChartAsSVG)



{-| Chart can (1)  display a line graph of data presented as a list of pairs of floats,
assumed to be in increasing order of their x-coordinates;
(2) display a bar graph of data presented as a list of floats.

    lineChart : GraphAttributes -> List Point -> Html msg

    barChart : GraphAttributes -> List Float -> Html msg

**Example (Line Chart).**  Let

    data =
        [(0,0), (10, 10), (20,0), (30,15), (40,0)]

    graphAttributes =
         {   graphHeight = 100
           , graphWidth = 400
           , options = [ ]
         }

    lineChart graphAttributes data

For more control over the part of the data displayed, use `lineChartWithDataWindow`.
To customize the appearance of the graph, use the options field -- change the color of the
line, place tick marks on the x and y axes.  For example, one could say `options = [Color "blue"]`.

**Example (Bar Chart).** Let

    data = [5, 10, 20, 30, 20, 20, 5]

    graphAttributes =
        {   graphHeight = 100
          , graphWidth = 400
          , options = [Color "rgb(200,0,0)", DeltaX 15,  XTickmarks 2, YTickmarks 5]
        }

    barChart graphAttributes data

@docs lineChart, lineChartWithDataWindow,  barChart,  Point, DataWindow, GraphAttributes, Option, lineChartAsSVG, lineChartAsSVGWithDataWindow, barChartAsSVG


-}


import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, svg, text, text_)
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
-}
type Option =
    Color String
  | XTickmarks Int
  | YTickmarks Int
  | DeltaX Float


{-| A DataWindow is a rectangle which determines
the x and y ranges of the data to be displayed..
-}
type alias DataWindow =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
  }



{-|  The data to be graphed by Chart.asHtml is
a List Point.

-}
type alias Point = (Float, Float)


type alias Segment = (Point, Point)



type alias ScaleFactor =
    {
       kx : Float
      ,ky : Float
    }


{-| Render a list of points to Html as a line chart using the parameters
of GraphAttributes.
-}
lineChart : GraphAttributes -> List Point -> Html msg
lineChart ga data =
  lineChartWithDataWindow (getDataWindow data) ga data


{-| Render a list of points to SVG as a line chart using the parameters
of GraphAttributes.
-}
lineChartAsSVG : GraphAttributes ->  List Point -> Svg msg
lineChartAsSVG ga data =
   lineChartAsSVGWithDataWindow (getDataWindow data) ga data

{-| Render a list of points to Html as a line chart using the parameters
of GraphAttributes and DataWindow.
-}
lineChartWithDataWindow : DataWindow -> GraphAttributes ->List Point -> Html msg
lineChartWithDataWindow dw ga  data =
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
lineChartAsSVGWithDataWindow: DataWindow -> GraphAttributes ->  List Point -> Svg msg
lineChartAsSVGWithDataWindow dw ga data =
    let
      scaleFactor = getScaleFactor dw ga

      render : List Point -> Svg msg
      render data_ =
          data_
            |> translate (-dw.xMin, dw.yMax)
            |> rescale scaleFactor
            |> segments
            |> segmentsToSVG ga.options

      renderPlain : List Point -> Svg msg
      renderPlain data_ =
                data_
                  |> translate (-dw.xMin, dw.yMax)
                  |> rescale scaleFactor
                  |> segments
                  |> segmentsToSVG []

      theData = data |> render

      abscissa = [(dw.xMin,0), (dw.xMax,0)]  |> renderPlain

      ordinate = [(dw.xMin, dw.yMin), (dw.xMin, dw.yMax)]  |> renderPlain

      boundingBox_ = boundingBox ga.options dw |> renderPlain

      xTickMarks_ = makeXTickMarks scaleFactor renderPlain dw (xTickmarks ga.options)

      yTickMarks_ = makeYTickMarks scaleFactor renderPlain dw (yTickmarks ga.options)

      xLabels = makeXLabels scaleFactor dw (xTickmarks ga.options)

      yLabels = makeYLabels scaleFactor dw (yTickmarks ga.options)

    in
      g [] [theData, abscissa, ordinate, boundingBox_, xTickMarks_, yTickMarks_, xLabels, yLabels]


--
-- BARCHART
--


{-| A GraphAttributes value defines the size on
the screen occupied by the graph, the color of the
line, and the distance from the leading edge of
one bar to the next.

-}
type alias BarGraphAttributes =
    { dx : Float
    , color : String
    , barHeight : Float
    , graphWidth : Float
    }


{-| Render a list of numbers to Html as a bar chart using the parameters
of GraphAttributes and DataWindow.  If desired, the data window
can be set from the list of points using getDataWindow.
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
of GraphAttributes and DataWindow.  If desired, the data window
can be set from the list of points using getDataWindow.
-}
barChartAsSVG : GraphAttributes -> List Float -> Svg msg
barChartAsSVG ga data =
    let
            barWidth =
                0.8 * (deltaX ga.options)

            gbar =
                \( x, y ) -> barRect (lineColor ga.options) barWidth ga.graphHeight x y

            ordinate = segmentToSVG [] ((0,0), (0, ga.graphHeight))

            abscissa = segmentToSVG [] ((0,0), (ga.graphWidth, 0))

            xTickmarks2 = bxTickmarks ga

            yTickmarks2 = byTickmarks ga

            (yMax, preparedData) = prepare (deltaX ga.options) data

            yLabels = bMakeYLabels yMax ga
    in
      List.map gbar preparedData ++ [abscissa, ordinate, xTickmarks2, yTickmarks2, yLabels]
        |> g []


byTickmark : Float -> Svg msg
byTickmark y =
    segmentToSVG [] ((0,y), (-8, y))

byTickmarks : GraphAttributes -> Svg msg
byTickmarks ga =
  let
     n = yTickmarks ga.options
  in
     List.range 0 (n - 1)
       |> List.map (\k -> (toFloat k) * ga.graphHeight / (toFloat (n - 1)))
       |> List.map byTickmark
       |> g []

bxTickmark : Float -> Svg msg
bxTickmark x =
    segmentToSVG [] ((x,0), (x, -8))

bxTickmarks : GraphAttributes -> Svg msg
bxTickmarks ga =
  let
     dx = (toFloat <| xTickmarks ga.options) * (deltaX ga.options)
     n = round <| ga.graphWidth / dx
  in
     List.range 0 (n - 1)
       |> List.map (\k -> (toFloat k) * dx)
       |> List.map bxTickmark
       |> g []

bMakeYLabel :  (Float, Float) -> Float -> Svg msg
bMakeYLabel (yMax, graphHeight) y =
    let
        label = String.fromFloat <| roundTo 1 y
    in
    text_ [ SA.transform <| "translate(0," ++ "-3" ++ ") scale(1,-1)"
           , SA.x <| "-30"
           , SA.y <| String.fromFloat (-y * graphHeight / yMax)
           , SA.fontSize "9px"]
         [ text label ]

bMakeYLabels : Float -> GraphAttributes -> Svg msg
bMakeYLabels  yMax ga =
    let
        n = yTickmarks ga.options
    in
    case n == 0 of
            True -> g [] []
            False ->
              List.range 0 (n - 1)
                     |> List.map (\k -> (toFloat k) * yMax / (toFloat (n - 1)))
                     |> List.map (bMakeYLabel (yMax, ga.graphHeight))
                |> g []

prepare : Float -> List Float -> (Float, List ( Float, Float ))
prepare dx data =
    let
        xs =
            xCoordinates (List.length data) dx

        ymax =
            List.maximum data |> Maybe.withDefault 1

        ys =
            List.map (\y -> y / ymax) data
    in
    (ymax, List.map2 Tuple.pair xs ys)

xCoordinates : Int -> Float -> List Float
xCoordinates n dx =
    List.map (\i -> toFloat i * dx) (List.range 0 n)

barRect : String -> Float -> Float -> Float -> Float -> Svg msg
barRect color barWidth barHeight x fraction =
    rect
        [ SA.width <| String.fromFloat barWidth
        , SA.height <| String.fromFloat <| fraction * barHeight
        , SA.x <| String.fromFloat x
        , SA.fill color
        ]
        []


--
-- INTERNAL
--

rescale : (Float, Float) -> List Point -> List Point
rescale (kx, ky) data =
    List.map (\(x,y) -> (kx * x, ky * y)) data

translate : (Float, Float) -> List Point -> List Point
translate (dx, dy) data =
    List.map (\(x,y) -> (x + dx, y + dy)) data


boundingBox : List Option -> DataWindow -> List Point
boundingBox options dw =
    case (xTickmarks options, yTickmarks options) of
        (0, 0) -> [ ]
        (_, _) ->
          [(dw.xMin, dw.yMin), (dw.xMax, dw.yMin), (dw.xMax, dw.yMax), (dw.xMin, dw.yMax), (dw.xMin, dw.yMin)]

-- BASIC SVG ELEMENT




segments : List a -> List (a, a)
segments list =
    let
      n = List.length list
    in
    List.map2 Tuple.pair (List.take (n - 1) list ) (List.drop 1 list)

{-| Create a DataWindow from a list of points. This will
be the smallest rectangle containing the data.

-}
getDataWindow : List Point -> DataWindow
getDataWindow pointList =
    let
        xs = List.map Tuple.first pointList
        ys = List.map Tuple.second pointList
        xMin = List.minimum xs |> Maybe.withDefault 0
        xMax = List.maximum xs |> Maybe.withDefault 0
        yMin = List.minimum ys |> Maybe.withDefault 0
        yMax = List.maximum ys |> Maybe.withDefault 0
    in
      { xMin = xMin
      , xMax = xMax
      , yMin = yMin
      , yMax = yMax
      }

getScaleFactor : DataWindow -> GraphAttributes -> (Float, Float)
getScaleFactor dataWindow gA =
    let
        kx = gA.graphWidth/(dataWindow.xMax - dataWindow.xMin)
        ky = gA.graphHeight/(dataWindow.yMax - dataWindow.yMin)
    in
     (kx, ky)



segmentToSVG : List Option -> Segment -> Svg msg
segmentToSVG options ((x1, y1), (x2, y2)) =
        line [ SA.x1 (String.fromFloat x1)
             , SA.y1 (String.fromFloat y1)
             , SA.x2 (String.fromFloat x2)
             , SA.y2 (String.fromFloat y2)
             , SA.stroke <| lineColor options
             , SA.strokeWidth "1" ] []


segmentsToSVG : List Option -> List Segment -> Svg msg
segmentsToSVG options  segmentList =
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
        DeltaX dx -> Just dx
        _ -> Nothing

lineColor : List Option -> String
lineColor options =
    findMap lineColor_ options |> Maybe.withDefault "rgb(40, 40, 40)"

lineColor_ : Option -> Maybe String
lineColor_ option =
    case option of
        Color str -> Just str
        _ -> Nothing


xTickmarks : List Option -> Int
xTickmarks options =
    findMap xTickmarks_ options |> Maybe.withDefault 0

xTickmarks_ : Option -> Maybe Int
xTickmarks_ option =
    case option of
        XTickmarks k -> Just k
        _ -> Nothing

yTickmarks : List Option -> Int
yTickmarks options =
    findMap yTickmarks_ options |> Maybe.withDefault 0

yTickmarks_ : Option -> Maybe Int
yTickmarks_ option =
    case option of
        YTickmarks k -> Just k
        _ -> Nothing


tickMarkLength = 7.5

makeYTickMark : Float -> DataWindow -> Float -> List Point
makeYTickMark kx dw y =
    [(dw.xMin,y), (dw.xMin - (tickMarkLength/kx),y)]

makeYLabel : (Float, Float) -> DataWindow -> Float -> Svg msg
makeYLabel (kx,ky) dw y =
    let
        dy = String.fromFloat (ky*(y - dw.yMin) - 3)
        label = String.fromFloat <| roundTo 1 y
    in
    text_ [ SA.transform <| "translate(0," ++ dy ++ ") scale(1,-1)"
           , SA.x <| String.fromFloat -30
           , SA.y <| "0"
           , SA.fontSize "9px"]
         [ text label ]

makeXLabels : (Float, Float) -> DataWindow -> Int -> Svg msg
makeXLabels (kx,ky) dw n =
    case n == 0 of
            True -> g [] []
            False ->
                List.range 0 (n - 1)
                  |> List.map (\k -> dw.xMin + (toFloat k) * (dw.xMax - dw.xMin)/(toFloat (n - 1)))
                  |> List.map (makeXLabel (kx,ky) dw)
                  |> (\x -> g [] x )

makeXLabel : (Float, Float) -> DataWindow -> Float -> Svg msg
makeXLabel (kx,ky) dw x =
    let
        dx = String.fromFloat (kx*(x - dw.xMin))
        label = String.fromFloat <| roundTo 1 x
    in
    text_ [ SA.transform <| "translate(" ++ dx ++ ",0) scale(1,-1)"
           , SA.x <| "-8"
           , SA.y <| "20"
           , SA.fontSize "9px"]
         [ text label ]

makeYLabels : (Float, Float) -> DataWindow -> Int -> Svg msg
makeYLabels (kx,ky) dw n =
    case n == 0 of
            True -> g [] []
            False ->
                List.range 0 (n - 1)
                  |> List.map (\k -> dw.yMin + (toFloat k) * (dw.yMax - dw.yMin)/(toFloat (n - 1)))
                  |> List.map (makeYLabel (kx,ky) dw)
                  |> (\x -> g [] x )

makeYTickMarks : (Float, Float) -> (List Point -> Svg msg) -> DataWindow -> Int -> Svg msg
makeYTickMarks (kx,ky) render dw n =
    case n == 0 of
        True -> []  |> List.map render |> (\x -> g [] x)
        False ->
            List.range 0 (n - 1)
              |> List.map (\k -> (toFloat k) * (dw.yMax - dw.yMin)/(toFloat (n - 1)))
              |> List.map (makeYTickMark kx dw)
              |> List.map (translate (0, -dw.yMax))
              |> List.map render
              |> (\x -> g [] x )

makeXTickMark : Float -> DataWindow -> Float -> List Point
makeXTickMark ky dw x =
    [(x, dw.yMin), (x, dw.yMin - (tickMarkLength/ky))]

makeXTickMarks : (Float, Float) -> (List Point -> Svg msg) -> DataWindow -> Int -> Svg msg
makeXTickMarks (kx, ky) render dw n =
    case n == 0 of
        True -> [] |> List.map render |> (\x -> g [] x)
        False ->
            List.range 0 (n - 1)
              |> List.map (\k -> (toFloat k) * (dw.xMax - dw.xMin)/(toFloat (n - 1)))
              |> List.map (makeXTickMark (ky) dw)
              |> List.map (translate (dw.xMin, 0))
              |> List.map render
              |> (\x -> g [] x)
--
-- UTILITY
--

findMap : (a -> Maybe b) -> List a -> Maybe b
findMap f list =
  case list of
    [] -> Nothing
    x::xs ->
      case f x of
        Just v -> Just v
        Nothing -> findMap f xs


roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)

