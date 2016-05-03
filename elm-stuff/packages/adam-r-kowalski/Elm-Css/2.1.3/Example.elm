module Css.Example where

import Window
import Signal exposing (map, map2, constant, foldp)
import Color exposing (Color, rgba, complement)
import Time exposing (fps)

import Html exposing (Html, div, text, pre, code, ul, li)
import Html.Attributes as Attributes exposing (style)

import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Shadow as Shadow
import Css.Background as Background
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Dimension as Dimension
import Css.Margin as Margin

import Css.Example.Util exposing (Example)
import Css.Example.Example1 exposing (example1)
import Css.Example.Example2 exposing (example2)
import Css.Example.Example3 exposing (example3)
import Css.Example.Example4 exposing (example4)


centered : Styles -> Styles
centered styles =
  styles
    |> display Display.Flex
    |> Flex.alignItems Flex.AICenter
    |> Flex.justifyContent Flex.JCCenter


middle : Styles -> Styles
middle styles =
  styles
    |> Flex.justifyContent Flex.JCAround
    |> display Display.Flex


wrap : Styles -> Styles
wrap styles =
  Flex.wrap Flex.Wrap styles


column : Styles -> Styles
column styles =
  Flex.direction Flex.Column styles


header : Html
header =
  let styles =
    []
      |> centered
      |> Shadow.box [ (0, 0, 10, 1, (rgba 0 0 0 1), False) ]
      |> Background.color (rgba 26 188 156 1)
      |> Text.color (rgba 255 255 255 1)
      |> Font.size 36
      |> Padding.all 10 0 10 0
  in
    div
    [ style styles ]
    [ text "Elm Css" ]


example : (Int, Int) -> Example -> Html
example (width, height) (name, description, codeString, view, explanation) =
  let styles =
    []
      |> Padding.all 20 20 0 20
      |> Dimension.maxWidth 1000
      |> Dimension.width width
  in
    div
    [ style styles ]
    [ div [ style <| Font.size 24 [] ] [ text name ]
    , div [ style <| Margin.top 10 [] ] [ text description ]
    , div
      [ style <| middle <| wrap <| Margin.bottom 10 [] ]
      [ pre [] [ code [ style <| Font.size 18 [] ] [ text codeString ] ]
      , view
      ]
    , explanation
    ]


view : (Int, Int) -> Float -> Html
view dimensions time =
  div
  []
  [ setViewport
  , header
  , div
    [ style <| column <| centered [] ]
    <| List.map (example dimensions)
        [ example1
        , example2
        , example3 dimensions
        , example4 time
        ]
  ]


getTime : Float -> Float -> Float
getTime delta accumulator =
  accumulator + (delta / 60)


main : Signal Html
main =
  map2 view Window.dimensions (foldp getTime 0 (fps 60))
