module Chart exposing (GraphAttributes, Option(..), DataWindow)


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