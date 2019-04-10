module LineGraph exposing(GraphAttributes, asHtml, asSVG)



{- (GraphAttributes, asHtml, asSVG) -}

{-| BarGraph displays a bar graph of data presented as a list of floats:

@docs GraphAttributes, asHtml, asSVG

-}

{- exposing (GraphAttributes, asHtml, asSVG) -}

import Element exposing (..)
import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SA


type alias GraphAttributes =
    { dx : Float
    , color : String
    , graphHeight : Float
    , graphWidth : Float
    }

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



asHtml : GraphAttributes -> List (Float, Float) -> Html msg
asHtml ga data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.graphHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 50)
        , SA.viewBox <| "-40 -20 " ++ String.fromFloat (ga.graphWidth + 50) ++ " " ++ String.fromFloat (ga.graphHeight + 40)
        ]
        [ asSVG ga data ]


asSVG : GraphAttributes -> List (Float, Float) -> Svg msg
asSVG gA data =
      g []
        [svgOfData gA data]



svgOfData : GraphAttributes -> List (Float, Float) -> Svg msg
svgOfData ga data =
    let
      dw = getDataWindow data
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
        [(0, dw.yMin), (0, dw.yMax)]
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



type alias Point = (Float, Float)
type alias Segment = (Point, Point)



segments : List a -> List (a, a)
segments list =
    let
      n = List.length list
    in
    List.map2 Tuple.pair (List.take (n - 1) list ) (List.drop 1 list)


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

