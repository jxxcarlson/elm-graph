module Main exposing (main)

{- This is a demo of the BarChart package
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Chart exposing(Option(..), barChart, lineChart)


--
-- DATA
--



lineData  : List (Float, Float)
lineData =
    [(-20, 0), (-10,-10), (0,0), (10, 10), (20,0), (30,15), (40,0), (50,-30), (60, 0)]

lineData2  : List (Float, Float)
lineData2 =
    [(0,0), (10, 10), (20,0), (30,15), (40,0)]

barData : List Float
barData = [5, 10, 20, 30, 20, 20, 5]

lineGraphAttributes =
    {   graphHeight = 100
      , graphWidth = 400
      , options = [ Color "blue", YTickmarks 4]
    }

lineGraphAttributes2 =
    {   graphHeight = 100
      , graphWidth = 400
      , options = [ Color "blue", XTickmarks 10, YTickmarks 5]
    }

barGraphAttributes =
    {   graphHeight = 100
      , graphWidth = 400
      , options = [Color "rgb(200,0,0)", DeltaX 15, YTickmarks 6, XTickmarks 2]
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
    { }


type Msg
    = NoOp



type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {  }, Cmd.none )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


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
             , row [] [ lineChart lineGraphAttributes lineData |> Element.html ]
             , row [] [ barChart barGraphAttributes barData |> Element.html ]

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



