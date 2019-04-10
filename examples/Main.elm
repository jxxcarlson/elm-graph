module Main exposing (main)

{- This is a demo of the BarGraph package
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
-- import BarGraph
import LineGraph


--
-- DATA
--

data : List Float
data =
    [ 0, 1, 2, 3, 2, 1, 0 ]

data2 : List (Float, Float)
data2 =
    [(0,0), (10, 10), (20,0)]

data3 : List (Float, Float)
data3 =
    [(-20, 0), (-10,-10), (0,0), (10, 10), (20,0), (30,15), (40,0), (50,-15), (60, 0)]


graphAttributes =
    { dx = 10
    , color = "blue"
    , graphHeight = 100
    , graphWidth = 400
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
        [ column [ centerX, centerY, spacing 20, padding 40, Background.color (rgb255 240 240 240) ]
            [ title "Bar Graph Demo"
            , LineGraph.asHtml graphAttributes data3 |> Element.html
           -- , BarGraph.asHtml graphAttributes data |> Element.html
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



