module GamblersRuin exposing (main)

{- This is a demo of the BarChart package
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Chart exposing(Option(..), barChart, lineChart, DataWindow)
import Time
import Random


--
-- DATA
--



lineGraphAttributes =
    {     graphWidth = 400
        , graphHeight = 100
        , options = [ Color "blue", XTickmarks 5, YTickmarks 5]
    }

dataWindow =
    { xMax = 400
    , xMin = 0
    , yMax = 30
    , yMin = -30
  }
--
-- APP
--

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {   timeSeries : List (Float, Float)
      , counter : Int}


type Msg
    = NoOp
    | Tick Time.Posix
    | NewRandomNumber Float



type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { timeSeries = [(0,10)], counter = 0 }, Cmd.none )


subscriptions model =
    Time.every 100 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Tick  _ ->
              ( model
              , Random.generate NewRandomNumber (Random.float -1 1)

              )

        NewRandomNumber r ->
           let
               (t,v) = List.head model.timeSeries |> Maybe.withDefault (-1,-1)
               vv = (t+1, v + r)
           in
            ( { model | timeSeries = windowedData dataWindow (vv::model.timeSeries), counter = model.counter + 1 }, Cmd.none)



windowedData : DataWindow -> List (Float, Float) -> List (Float, Float)
windowedData dw data =
    let
        n = round dw.xMax
    in
    case List.length data <= n of
        True -> data
        False ->
           data
             |> List.take n
             |> List.map (\(t,v) -> (t-1,v))

--
-- VIEW
--



view : Model -> Html Msg
view model =
    Element.layout [ ] (mainColumn model)


{-| This paragraph is where all the action is -}
mainColumn : Model -> Element Msg
mainColumn model =
      column mainColumnStyle
        [ column [ centerX, centerY, spacing 60, padding 40, Background.color (rgb255 240 240 240) ]
            [ title "Chart Demo"
             , row [] [ Chart.lineChartWithDataWindow dataWindow lineGraphAttributes  model.timeSeries |> Element.html ]
             --, row [] [ Chart.lineChart lineGraphAttributes  model.timeSeries |> Element.html ]

            ]
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]



mainColumnStyle =
    [
     height fill
    , width fill
    , Background.color (rgb255 80 80 80)
    , paddingXY 20 20
    ]



