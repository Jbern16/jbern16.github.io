module Css.Example.Example1 where

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


example1 : Example
example1 =
  ( "Example 1"
  , "Elm Css allows you to harness all the power of Css inside of Elm.
     This allows you to construct all of your styles dynamically.
     Here is an example of how we can create a reusable square component."
  , codeString
  , view
  , explanation
  )


codeString : String
codeString = """
main : Html
main =
  div
  [ style <| centered <| column <| flex [] ]
  <| List.map square [ 50, 60, 70, 80, 90 ]


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


centered : Styles -> Styles
centered styles =
  styles
    |> Flex.justifyContent Flex.JCCenter
    |> Flex.alignItems Flex.AICenter


column : Styles -> Styles
column styles =
  Flex.direction Flex.Column styles


square : Int ->  Html
square sideLength =
  let styles =
    []
      |> flex
      |> centered
      |> Margin.all 10 10 10 10
      |> Text.color (rgba 255 255 255 1)
      |> Background.color (rgba 255 0 0 1)
      |> Dimension.width sideLength
      |> Dimension.height sideLength
  in div
    [ style styles ]
    [ text <| toString sideLength ]
"""


explanation : Html
explanation =
  div
  [ style <| column <| Margin.bottom 50 [] ]
  [ div
    [ style <| Margin.bottom 10 [] ]
    [ text """
The first thing to notice is how every style is composable. We can mix and match
styles as a result of this allowing us to have progressively more and more
complex styles. Using this approach also allows for type checking in our Css.
This means it becomes a lot simpler to diagnose the cause of problems, and allows
the compiler to help you not make mistakes.
""" ]
  , div
    [ style <| Margin.bottom 10 [] ]
    [ text """
When creating simple styles it may be easy to see how it transforms into Css,
but as the scale of your projects increases this may no longer be the case.
For debugging purposes it may be desirable to see a representation of what
the Css looks like for a given style. It takes in the class name as well as a
list of styles.
""" ]
  , div
    [ style <| flex <| centered <| Font.size 18 [] ]
    [ code
      [ style <| Margin.bottom 10 [] ]
      [ text "Util.toCss \"centered\" <| centered []" ]
    ]
  , Util.toCss "centered" <| centered []
  ]

view : Html
view =
  div
  [ style <| centered <| column <| flex [] ]
  <| List.map square [ 50, 60, 70, 80, 90 ]


flex : Styles -> Styles
flex styles =
  display Display.Flex styles


centered : Styles -> Styles
centered styles =
  styles
    |> Flex.justifyContent Flex.JCCenter
    |> Flex.alignItems Flex.AICenter


column : Styles -> Styles
column styles =
  Flex.direction Flex.Column styles


square : Int ->  Html
square sideLength =
  let styles =
    []
      |> flex
      |> centered
      |> Margin.all 10 10 10 10
      |> Text.color (rgba 255 255 255 1)
      |> Background.color (rgba 255 0 0 1)
      |> Dimension.width sideLength
      |> Dimension.height sideLength
  in div
    [ style styles ]
    [ text <| toString sideLength ]
