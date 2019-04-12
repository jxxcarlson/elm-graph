module LineChart exposing(Point, GraphAttributes, Option(..), DataWindow
  , getDataWindow,  asHtml, asSVG, asHtmlWithDataWindow, asSVGWithDataWindow)



{-| LineChart displays a line graph of data presented as a list of pairs of floats.

    LineChart.asHtml : GraphAttributes -> List Point -> Html msg

**Example:**  Let

    data =
        [(0,0), (10, 10), (20,0), (30,15), (40,0)]

    graphAttributes =
         {   graphHeight = 100
           , graphWidth = 400
           , options = [ ]
         }

    LineChart.asHtml graphAttributes data

For more control over the part of the data displayed, use LineChart.asHtmlWithDataWindow.
To customize the appearance of the graph, use the options field -- change the color of the
line, place tick marks on the x and y axes.  For example, one could say options = [Color "blue"].

@docs Point,  GraphAttributes, Option, DataWindow, getDataWindow,  asHtml, asSVG, asHtmlWithDataWindow, asSVGWithDataWindow

-}


import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SA

{-|  The data to be graphed by LineChart.asHtml is
a List Point.

-}
type alias Point = (Float, Float)


type alias Segment = (Point, Point)


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
-}
type Option =
  Color String
  | XTickmarks Int
  | YTickmarks Int


{-| A DataWindow is a rectangle which determines
the x and y ranges of the data to be displayed..
-}
type alias DataWindow =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    }

type alias ScaleFactor =
    {
       kx : Float
      ,ky : Float
    }


{-| Render a list of points to Html as a line chart using the parameters
of GraphAttributes.
-}
asHtml : GraphAttributes -> List Point -> Html msg
asHtml ga data =
  asHtmlWithDataWindow (getDataWindow data) ga data


{-| Render a list of points to SVG as a line chart using the parameters
of GraphAttributes.
-}
asSVG : GraphAttributes ->  List Point -> Svg msg
asSVG ga data =
   asSVGWithDataWindow (getDataWindow data) ga data

{-| Render a list of points to Html as a line chart using the parameters
of GraphAttributes and DataWindow.
-}
asHtmlWithDataWindow : DataWindow -> GraphAttributes ->List Point -> Html msg
asHtmlWithDataWindow dw ga  data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 50)
        , SA.viewBox <| "-40 -20 " ++ String.fromFloat (ga.graphWidth + 50) ++ " " ++ String.fromFloat (ga.graphHeight + 40)
        ]
        [ asSVGWithDataWindow dw ga data ]


{-| Render a list of points to Svg as a line chart using the parameters
of GraphAttributes and DataWindow.
-}
asSVGWithDataWindow: DataWindow -> GraphAttributes ->  List Point -> Svg msg
asSVGWithDataWindow dw ga data =
    let
      scaleFactor = getScaleFactor dw ga

      render data_ =
          data_
            |> translate (-dw.xMin, dw.yMax)
            |> rescale scaleFactor
            |> segments
            |> segmentsToSVG ga.options

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

      yTickMarks_ = makeYTickMarks scaleFactor dw (yTickmarks ga.options) |> List.map renderPlain |> (\x -> g [] x)

      xTickMarks_ = makeXTickMarks scaleFactor  dw (xTickmarks ga.options) |> List.map renderPlain |> (\x -> g [] x)

    in
      g [] [theData, abscissa, ordinate, boundingBox_, xTickMarks_, yTickMarks_]


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


-- text_ [ SA.transform <| "translate(0," ++ dy ++ ") scale(1,-1)", SA.x <| String.fromFloat -40, SA.y <| "0" ] [ text label ]




makeYTickMarks : (Float, Float) -> DataWindow -> Int -> List (List Point)
makeYTickMarks (kx,ky) dw n =
    case n == 0 of
        True -> []
        False ->
            List.range 0 (n - 1)
              |> List.map (\k -> (toFloat k) * (dw.yMax - dw.yMin)/(toFloat (n - 1)))
              |> List.map (makeYTickMark kx dw)
              |> List.map (translate (0, -dw.yMax))

makeXTickMark : Float -> DataWindow -> Float -> List Point
makeXTickMark ky dw x =
    [(x, dw.yMin), (x, dw.yMin - (tickMarkLength/ky))]

makeXTickMarks : (Float, Float) -> DataWindow -> Int -> List (List Point)
makeXTickMarks (kx, ky) dw n =
    case n == 0 of
        True -> []
        False ->
            List.range 0 (n - 1)
              |> List.map (\k -> (toFloat k) * (dw.xMax - dw.xMin)/(toFloat (n - 1)))
              |> List.map (makeXTickMark (ky) dw)
              |> List.map (translate (dw.xMin, 0))
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

