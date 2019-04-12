module LineChart exposing(GraphAttributes, DataWindow, getDataWindow,  asHtml, asSVG)



{-| LineChart displays a line graph of data presented as a list of floats:

@docs GraphAttributes, DataWindow, getDataWindow,  asHtml, asSVG

-}




import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SA


type alias Point = (Float, Float)
type alias Segment = (Point, Point)


{-| A GraphAttributes value defines the size on
the screen occupied by the graph and the color of the
line.

-}
type alias GraphAttributes =
    { color : String
    , graphHeight : Float
    , graphWidth : Float
    }

{-| A DataWindow is a rectangle containing the data.
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
of GraphAttributes and DataWindow.  If desired, the data window
can be set from the list of points using getDataWindow.
-}
asHtml : GraphAttributes -> DataWindow -> List Point -> Html msg
asHtml ga dw data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 50)
        , SA.viewBox <| "-40 -20 " ++ String.fromFloat (ga.graphWidth + 50) ++ " " ++ String.fromFloat (ga.graphHeight + 40)
        ]
        [ asSVG ga dw data ]


{-| Render a list of points to Svg as a line chart using the parameters
of GraphAttributes and DataWindow.  If desired, the data window
can be set from the list of points using getDataWindow.
-}
asSVG : GraphAttributes -> DataWindow -> List (Float, Float) -> Svg msg
asSVG gA dw data =
      g []
        [svgOfData gA dw data]



svgOfData : GraphAttributes -> DataWindow -> List (Float, Float) -> Svg msg
svgOfData ga dw data =
    let
      scaleFactor = getScaleFactor dw ga
      theData =
          data
                |> translate (-dw.xMin, dw.yMax)
                |> rescale scaleFactor
                |> segments
                |> segmentsToSVG
      abscissa =
          [(dw.xMin,0), (dw.xMax,0)]
                |> translate (-dw.xMin, dw.yMax)
                |> rescale scaleFactor
                |> segments
                |> segmentsToSVG

      ordinate =
        [(dw.xMin, dw.yMin), (dw.xMin, dw.yMax)]
              |> translate (-dw.xMin, dw.yMax)
              |> rescale scaleFactor
              |> segments
              |> segmentsToSVG

    in
      g [] [theData, abscissa, ordinate]


rescale : (Float, Float) -> List Point -> List Point
rescale (kx, ky) data =
    List.map (\(x,y) -> (kx * x, ky * y)) data

translate : (Float, Float) -> List Point -> List Point
translate (dx, dy) data =
    List.map (\(x,y) -> (x + dx, y + dy)) data


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



segmentToSVG : Segment -> Svg msg
segmentToSVG ((x1, y1), (x2, y2)) =
        line [ SA.x1 (String.fromFloat x1)
             , SA.y1 (String.fromFloat y1)
             , SA.x2 (String.fromFloat x2)
             , SA.y2 (String.fromFloat y2)
             , SA.stroke "rgb(80,80,80)"
             , SA.strokeWidth "1" ] []

segmentsToSVG : List Segment -> Svg msg
segmentsToSVG segmentList =
    segmentList |> List.map segmentToSVG |> (\x -> g [] x)

--
-- UTILITY
--

roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)

