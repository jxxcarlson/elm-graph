module LineGraph exposing(..)



{- (GraphAttributes, asHtml, asSVG) -}

{-| BarGraph displays a bar graph of data presented as a list of floats:

@docs GraphAttributes, asHtml, asSVG

-}

{- exposing (GraphAttributes, asHtml, asSVG) -}

import Element exposing (..)
import Html exposing (Html)
import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SA

data2 : List Point
data2 =
    [(0,0), (100, 100), (200,0)]

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
        , SA.viewBox <| "-35 -20 " ++ String.fromFloat (ga.graphWidth + 40) ++ " " ++ String.fromFloat (ga.graphHeight + 20)
        ]
        [ asSVG ga data ]


asSVG : GraphAttributes -> List (Float, Float) -> Svg msg
asSVG gA data =
    let
        offset =
            String.fromFloat axesOffset

        xMax_ =
            List.maximum (List.map Tuple.first data) |> Maybe.withDefault 0
        yMax =
            List.maximum (List.map Tuple.second data) |> Maybe.withDefault 0

        xyMaxAsString =
                    String.fromFloat (xMax_ |> roundTo 2)

        xMaxHalfAsString =
                    String.fromFloat (xMax_ / 2 |> roundTo 2)

        yMaxAsString =
            String.fromFloat (yMax |> roundTo 2)

        yMaxHalfAsString =
            String.fromFloat (yMax / 2 |> roundTo 2)
    in
    g [ SA.transform <| "translate(" ++ offset ++ "," ++ offset ++ ")" , SA.fontSize "12px" ] <|
        [svgOfData gA data]
            ++ [ abscissa gA, ordinate gA, yTickMark gA 0.0 "0  ", yTickMark gA 0.5 yMaxHalfAsString, yTickMark gA 1.0 yMaxAsString ]


svgOfData : GraphAttributes -> List (Float, Float) -> Svg msg
svgOfData ga data =
    let
      dw = getDataWindow data
      scaleFactor = getScaleFactor dw ga
      scaledData = rescale  scaleFactor data
    in
    scaledData
      |> segments
      |> segmentsToSVG


rescale : ScaleFactor -> List Point -> List Point
rescale f data =
    List.map (\(x,y) -> (f.kx * x, f.ky * y)) data

-- AXES



axesOffset =
    2


abscissa : GraphAttributes -> Svg msg
abscissa gA =
    let
        offset =
            String.fromFloat -axesOffset
    in
    line [ SA.x1 offset, SA.y1 offset, SA.x2 <| String.fromFloat gA.graphWidth, SA.y2 offset, SA.stroke "rgb(80,80,80)", SA.strokeWidth "2" ] []


ordinate : GraphAttributes -> Svg msg
ordinate gA =
    let
        offset =
            String.fromFloat -axesOffset
    in
    line [ SA.x1 offset, SA.y1 offset, SA.y2 <| String.fromFloat gA.graphHeight, SA.x2 offset, SA.stroke "rgb(80,80,80)", SA.strokeWidth "2" ] []


yTickMark : GraphAttributes -> Float -> String -> Svg msg
yTickMark gA height label =
    let
        dy =
            String.fromFloat <| gA.graphHeight * height - 3
    in
    g []
        [ line
            [ SA.x1 <| String.fromFloat -axesOffset
            , SA.y1 <| String.fromFloat <| height * gA.graphHeight
            , SA.x2 <| String.fromFloat (-axesOffset - 10)
            , SA.y2 <| String.fromFloat <| height * gA.graphHeight
            , SA.stroke "rgb(80,80,80)"
            , SA.strokeWidth "1"
            ]
            []
        , text_ [ SA.transform <| "translate(0," ++ dy ++ ") scale(1,-1)", SA.x <| String.fromFloat -40, SA.y <| "0" ] [ text label ]
        ]



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

getScaleFactor : DataWindow -> GraphAttributes -> ScaleFactor
getScaleFactor dataWindow gA =
    let
        kx = gA.graphWidth/(dataWindow.xMax - dataWindow.xMin)
        ky = gA.graphHeight/(dataWindow.yMax - dataWindow.yMin)
    in
     {kx = kx, ky = ky}



segmentToSVG : Segment -> Svg msg
segmentToSVG ((x1, y1), (x2, y2)) =
        line [ SA.x1 (String.fromFloat x1)
             , SA.y1 (String.fromFloat y1)
             , SA.x2 (String.fromFloat x2)
             , SA.y2 (String.fromFloat y2)
             , SA.stroke "rgb(80,80,80)"
             , SA.strokeWidth "2" ] []

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

