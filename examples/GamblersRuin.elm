module GamblersRuin exposing (main)

{- This is a demo of the BarChart package
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import SimpleChart exposing(Option(..), barChart, lineChart, DataWindow)
import Time
import Random


--
-- DATA
--

initialStake = 5.0
maximumWinnings = 35.0

lineGraphAttributes =
    {     graphWidth = 400
        , graphHeight = 100
        , options = [ Color "blue", YTickmarks 8, XTickmarks 5]
    }

--  options = [ Color "blue", XTickmarks 5, YTickmarks 5]

dataWindow =
    { xMax = 200
    , xMin = 0
    , yMax = maximumWinnings
    , yMin = 0
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
      , counter : Int
      , state : State}

type State = Waiting | Running | Paused | Finished

type Msg
    = NoOp
    | Tick Time.Posix
    | NewRandomNumber Float
    | Toggle
    | Restart




type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { timeSeries = [(0,initialStake)]
      , counter = 0
      , state = Waiting
     }, Cmd.none )


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

        NewRandomNumber r_ ->
           let
               r = case r_ > 0 of
                   True -> 1.0
                   False -> -1.0
               (t,v) = List.head model.timeSeries |> Maybe.withDefault (-1,-1)
               newV = v + r
               vv = (t+1, newV)
               nextTimeSeries =
                  case model.state of
                  Running -> windowedData dataWindow (vv::model.timeSeries)
                  _ -> model.timeSeries
               nextState = case newV <= 0 || newV >= maximumWinnings of
                   True -> Finished
                   False -> model.state
               nextCounter = case model.state of
                 Running -> model.counter + 1
                 _ -> model.counter
           in
           ( { model | timeSeries = nextTimeSeries, counter = nextCounter, state = nextState }, Cmd.none)

        Toggle ->
           case model.state of
               Waiting -> ({ model | state = Running }, Cmd.none)
               Running -> ({ model | state = Paused }, Cmd.none)
               Paused -> ({ model | state = Running }, Cmd.none)
               Finished -> ({ model | state = Finished }, Cmd.none)

        Restart ->
            ( { model | counter = 0, timeSeries = [(0,initialStake)], state = Running}, Cmd.none)



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
            [ column [spacing 8, centerX] [
               title "Gambler's Ruin"
              , row [Font.size 14] [text <| "Get to " ++ (String.fromFloat maximumWinnings) ++ " points for a 1000 dollar bonus"]
            ]
             , row [] [ SimpleChart.lineChartWithDataWindow dataWindow lineGraphAttributes  model.timeSeries |> Element.html ]
             , row [spacing 12] [startButton model, reStartButton model, status model, el [Font.bold, Font.size 16] (text <| message model)]

            ]
        ]

status : Model -> Element msg
status model =
    let
        (n, v) =  List.head model.timeSeries |> Maybe.withDefault (-1,-1)
    in
    el [Font.size 16] (text <| String.fromInt model.counter ++ ", " ++ (String.padRight 4 ' ' <| String.fromFloat (roundTo 2 v)))


message : Model -> String
message model =
    case model.state of
        Finished ->
            let
                (n, v) = List.head model.timeSeries |> Maybe.withDefault (0,0)
            in
              case v == 0 of
                  True -> "Sorry, you lost everything :("
                  False -> "Yay!! You win the jackpot!"
        _ -> ""

startButton : Model -> Element Msg
startButton model =
    Input.button [Border.width 1, Border.rounded 8, padding 8] {
      onPress = Just Toggle
      , label = el [Font.size 16] (text <| stateAsString model.state)
    }

reStartButton : Model -> Element Msg
reStartButton model =
    case model.state == Finished of
        False -> Element.none
        True ->
            Input.button [Border.width 1, Border.rounded 8, padding 8] {
              onPress = Just Restart
              , label = el [Font.size 16] (text "Restart")
            }

stateAsString : State -> String
stateAsString state =
    case state of
        Waiting -> "Ready"
        Running -> "Running"
        Paused -> "Paused"
        Finished -> "Finished"

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