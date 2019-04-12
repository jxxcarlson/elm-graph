## LineChart

LineChart displays a line graph of data presented as a list of pairs of floats.

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

For more control over the part of the data displayed, use `LineChart.asHtmlWithDataWindow`.
To customize the appearance of the graph, use the options field -- change the color of the
line, place tick marks on the x and y axes.  For example, one could say `options = [Color "blue"]`.