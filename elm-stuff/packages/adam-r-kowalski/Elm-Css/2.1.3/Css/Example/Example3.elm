module Css.Example.Example3 where

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
import Css.Dimension as Dimension
import Css.Util as Util
import Css.Example.Util exposing (Example)


example3 : (Int, Int) -> Example
example3 (width, height) =
  ( "Example 3"
  , "Lets expand on our idea and tackle another common challenge we face.
     Mobile is a huge industry and we must take this into consideration.
     Allowing our websites to react seamlessly to the size of the device the user
     is viewing is a difficult problem that is made a little easier with signals."
  , codeString
  , view (width, height)
  , explanation
  )


codeString : String
codeString = """
main : Signal Html
main =
  view <~ Window.dimensions


view : (Int, Int) -> Html
view (width, height) =
  div
  [ style
    <| column
    <| Text.color (rgba 255 255 255 1 ) []
  ]
  [ setViewport
  , div
    [ style <| end [] ]
    [ text "header" ]
  , div
    [ style <| responsiveRow width [] ]
    [ div
      [ style <| side width [] ]
      [ text "left" ]
    , div
      [ style <| content [] ]
      [ div [] [ text "content" ] ]
    , div
      [ style <| side width [] ]
      [ text "right" ]
    ]
  , div
    [ style <| end [] ]
    [ text "footer" ]
  ]


medium : Int
medium =
  900


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


responsiveRow : Int -> Styles -> Styles
responsiveRow width styles =
  if width < medium column <| flex styles
     else flex styles


end : Styles -> Styles
end styles =
  styles
    |> Dimension.height 50
    |> Background.color (rgba 26 188 156 1)
    |> centered


side : Int -> Styles -> Styles
side width styles =
  let w = if width < medium then 200 else 50
      h = Dimension.height 50 styles
      s = if width < medium then h else styles
  in
    s
      |> Dimension.width w
      |> Background.color (rgba 52 152 219 1)
      |> centered


content : Styles -> Styles
content styles =
  styles
    |> Dimension.width 200
    |> Dimension.height 200
    |> Background.color (rgba 52 73 94 1)
    |> centered


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
If we know the size of the screen we can create different styles based on
the dimensions, and allow our site to feel natural regardless screen size and
resolution.
""" ]
  ]


view : (Int, Int) -> Html
view (width, height) =
  div
  [ style
    <| column
    <| Text.color (rgba 255 255 255 1 ) [] ]
  [ div [ style <| end [] ] [ text "header" ]
  , div
    [ style <| responsiveRow width [] ]
    [ div
      [ style <| side width [] ]
      [ text "left" ]
    , div
      [ style <| content [] ]
      [ div [] [ text "content" ] ]
    , div
      [ style <| side width [] ]
      [ text "right" ]
    ]
  , div [ style <| end [] ] [ text "footer" ]
  ]


medium : Int
medium =
  900


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


responsiveRow : Int -> Styles -> Styles
responsiveRow width styles =
  if width < medium then column <| flex styles
  else flex styles


end : Styles -> Styles
end styles =
  styles
    |> Dimension.height 50
    |> Background.color (rgba 26 188 156 1)
    |> centered


side : Int -> Styles -> Styles
side width styles =
  let w = if width < medium then 200 else 50
      h = Dimension.height 50 styles
      s = if width < medium then h else styles
  in
    s
      |> Dimension.width w
      |> Background.color (rgba 52 152 219 1)
      |> centered


content : Styles -> Styles
content styles =
  styles
    |> Dimension.width 200
    |> Dimension.height 200
    |> Background.color (rgba 52 73 94 1)
    |> centered


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
