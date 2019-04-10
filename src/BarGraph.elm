module BarGraph exposing (GraphAttributes, asHtml, asSVG)

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
    , barHeight : Float
    , graphWidth : Float
    }


asHtml : GraphAttributes -> List Float -> Html msg
asHtml ga data =
    svg
        [ SA.transform "scale(1,-1)"
        , SA.height <| String.fromFloat (ga.barHeight + 40)
        , SA.width <| String.fromFloat (ga.graphWidth + 40)
        , SA.viewBox <| "-60 -20 " ++ String.fromFloat (ga.graphWidth + 40) ++ " " ++ String.fromFloat (ga.barHeight + 20)
        ]
        [ asSVG ga data ]


asSVG : GraphAttributes -> List Float -> Svg msg
asSVG gA data =
    let
        offset =
            String.fromFloat axesOffset

        yMax =
            List.maximum data |> Maybe.withDefault 0

        yMaxAsString =
            String.fromFloat (yMax |> roundTo 2)

        yMaxHalfAsString =
            String.fromFloat (yMax / 2 |> roundTo 2)
    in
    g [ SA.transform <| "translate(" ++ offset ++ "," ++ offset ++ ")" , SA.fontSize "12px" ] <|
        svgOfData gA data
            ++ [ abscissa gA, ordinate gA, yTickMark gA 0.0 "0  ", yTickMark gA 0.5 yMaxHalfAsString, yTickMark gA 1.0 yMaxAsString ]


svgOfData : GraphAttributes -> List Float -> List (Svg msg)
svgOfData ga data =
    let
        barWidth =
            0.8 * ga.dx

        gbar =
            \( x, y ) -> barRect ga.color barWidth ga.barHeight x y
    in
    List.map gbar (prepare ga.dx data)



-- PREPARE DATA


testData : List Float
testData =
    [ 0, 1, 2, 3, 2, 1, 0 ]


prepare : Float -> List Float -> List ( Float, Float )
prepare dx data =
    let
        xs =
            xCoordinates (List.length data) dx

        ymax =
            List.maximum data |> Maybe.withDefault 1

        ys =
            List.map (\y -> y / ymax) data
    in
    List.map2 Tuple.pair xs ys



-- COMPUTE LIST OF X COORDINATES
{-
   Suggestions from Ian:

   > List.map (\i -> toFloat i / 10) (List.range 0 4)
   [0,0.1,0.2,0.3,0.4] : List Float

   > List.map (\i -> toFloat i / 1000) (List.range 0 1000000) |> List.reverse |> List.take 5 |> List.reverse
   [999.996,999.997,999.998,999.999,1000]

-}


xCoordinates : Int -> Float -> List Float
xCoordinates n dx =
    List.map (\i -> toFloat i * dx) (List.range 0 n)


xMax : Float -> List Float -> Float
xMax dx data =
    let
        n =
            List.length data |> toFloat
    in
    (n - 1) * dx


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
    line [ SA.x1 offset, SA.y1 offset, SA.y2 <| String.fromFloat gA.barHeight, SA.x2 offset, SA.stroke "rgb(80,80,80)", SA.strokeWidth "2" ] []


yTickMark : GraphAttributes -> Float -> String -> Svg msg
yTickMark gA height label =
    let
        dy =
            String.fromFloat <| gA.barHeight * height - 3
    in
    g []
        [ line
            [ SA.x1 <| String.fromFloat -axesOffset
            , SA.y1 <| String.fromFloat <| height * gA.barHeight
            , SA.x2 <| String.fromFloat (-axesOffset - 10)
            , SA.y2 <| String.fromFloat <| height * gA.barHeight
            , SA.stroke "rgb(80,80,80)"
            , SA.strokeWidth "1"
            ]
            []
        , text_ [ SA.transform <| "translate(0," ++ dy ++ ") scale(1,-1)", SA.x <| String.fromFloat -40, SA.y <| "0" ] [ text label ]
        ]



-- BASIC SVG ELEMENT


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
-- UTILITY
--

roundTo : Int -> Float -> Float
roundTo k x =
    let
        kk =
            toFloat k
    in
    x * 10.0 ^ kk |> round |> toFloat |> (\y -> y / 10.0 ^ kk)
