module Css.Example.Example4 where

import Color exposing (rgba)

import Html exposing (Html, div, text, code)
import Html.Attributes exposing (style)

import Css exposing (Styles)
import Css.Display as Display exposing (display)
import Css.Flex as Flex
import Css.Font as Font
import Css.Text as Text
import Css.Margin as Margin
import Css.Background as Background
import Css.Gradient as Gradient
import Css.Dimension as Dimension
import Css.Util as Util
import Css.Example.Util exposing (Example)
import Css.Transform3 exposing (..)


example4 : Float -> Example
example4 time =
  ( "Example 4"
  , "CSS3 also allows you to do basic animation as well as transformations.
     While the transformations are very useful, I think that animations should
     be handled using signals, by knowing how much time has passed since a given
     point in the past."
  , codeString
  , view time
  , explanation
  )


codeString : String
codeString = """
main : Signal Html
main =
  view <~ (foldp getTime 0 (fps 60))


getTime : Float -> Float -> Float
getTime delta accumulator =
  accumulator + (delta / 60)


view : Float -> Html
view time =
  div
  [ style
    <| centered
    <| Text.color (rgba 255 255 255 1 ) [] ]
  [ setViewport
  , div
    [ style
      <| centered
      <| Dimension.width 200
      <| Dimension.height 200
      <| transform3 [ rotate3 1 1 1 time ]
      <| Gradient.linear 45 [ rgba 255 0 0 1, rgba 0 0 255 1 ] []
    ]
    [ div [] [ text "rotating square" ] ]
  ]


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


column : Styles -> Styles
column styles =
  styles
    |> flex
    |> Flex.direction Flex.Column


centered : Styles -> Styles
centered styles =
  styles
    |> flex
    |> Flex.alignItems Flex.AICenter
    |> Flex.justifyContent Flex.JCCenter
"""


explanation : Html
explanation =
  div
  [ style <| column [] ]
  [ div
    [ style <| Margin.bottom 10 [] ]
    [ text """
Here we can see that we can apply a background gradient to the element using
an angle of application as well as a list of colors to use. We also use the
time since the application began, normalized to return the current seconds.
This is useful to apply a 3D rotation to the element.
""" ]
  ]


view : Float -> Html
view time =
  div
  [ style
    <| centered
    <| Text.color (rgba 255 255 255 1 ) [] ]
  [ div
    [ style
      <| centered
      <| Dimension.width 200
      <| Dimension.height 200
      <| transform3 [ rotate3 1 1 1 time ]
      <| Gradient.linear 45 [ rgba 255 0 0 1, rgba 0 0 255 1 ] []
    ]
    [ div [] [ text "rotating square" ] ]
  ]


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


column : Styles -> Styles
column styles =
  styles
    |> flex
    |> Flex.direction Flex.Column


centered : Styles -> Styles
centered styles =
  styles
    |> flex
    |> Flex.alignItems Flex.AICenter
    |> Flex.justifyContent Flex.JCCenter
