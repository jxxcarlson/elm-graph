module Utilities exposing (..)

import Svg exposing (Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as SA

segments : List a -> List (a, a)
segments list =
    let
      n = List.length list
    in
    List.map2 Tuple.pair (List.take (n - 1) list ) (List.drop 1 list)


segmentToSVG : ((Float, Float), (Float, Float)) -> Svg msg
segmentToSVG ((x1, y1), (x2, y2)) =
        line [ SA.x1 (String.fromFloat x1)
             , SA.y1 (String.fromFloat y1)
             , SA.x2 (String.fromFloat x2)
             , SA.y2 (String.fromFloat y2)
             , SA.stroke "rgb(80,80,80)"
             , SA.strokeWidth "2" ] []

segmentsToSVG : List ((Float, Float), (Float, Float)) -> Svg msg
segmentsToSVG segmentList =
    segmentList |> List.map segmentToSVG |> (\x -> g [] x)