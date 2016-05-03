module Css.Border where

{-| Sets the border properties for all four sides at the same time.
You can set the color, style, width, and border radius

#Functions
@docs color, style, width, radius
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles)
import Css.Border.Bottom as BorderBottom
import Css.Border.Left as BorderLeft
import Css.Border.Right as BorderRight
import Css.Border.Top as BorderTop
import Css.Border.Style as BorderStyle exposing (Style)


{-| Sets the color of all four borders

    -- Sets all border colors to red
    import Css.Border as Border

    {- [ ("border-bottom-color", "rgba(255, 0, 0, 1)")
       , ("border-left-color", "rgba(255, 0, 0, 1)")
       , ("border-right-color", "rgba(255, 0, 0, 1)")
       , ("border-top-color", "rgba(255, 0, 0, 1)")
       ]
    -}
    Border.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  styles
    |> BorderBottom.color c
    |> BorderLeft.color c
    |> BorderRight.color c
    |> BorderTop.color c


{-| Sets the style of all four borders

    -- Sets the border style to dashed
    import Css.Border.Style as BorderStyle
    import Css.Border as Border

    {- [ ("border-bottom-style", "dashed")
       , ("border-left-style", "dashed")
       , ("border-right-style", "dashed")
       , ("border-top-style", "dashed")
       ]
    -}
    Border.style BorderStyle.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  styles
    |> BorderBottom.style s
    |> BorderLeft.style s
    |> BorderRight.style s
    |> BorderTop.style s


{-| Sets the width of all four borders

    -- Sets the border width to 5 pixels
    import Css.Border as Border

    {- [ ("border-bottom-width", "5px")
       , ("border-left-width", "5px")
       , ("border-right-width", "5px")
       , ("border-top-width", "5px")
       ]
    -}
    Border.width 5 []
-}
width : number -> number -> number -> number -> Styles -> Styles
width t r b l styles =
  styles
    |> BorderBottom.width b
    |> BorderLeft.width l
    |> BorderRight.width r
    |> BorderTop.width t


{-| Sets the radius of the
top left, top right, bottom right, and bottom left.

    -- Sets the radius of all four sides to 25 pixels
    import Css.Border as Border

    {- [ ("border-bottom-left-radius", "25px")
       , ("border-bottom-right-radius", "25px")
       , ("border-top-left-radius", "25px")
       , ("border-top-right-radius", "25px")
       ]
    -}
    Border.radius 25 25 25 25 []
-}
radius : number -> number -> number -> number -> Styles -> Styles
radius tl tr br bl styles =
  styles
    |> BorderTop.leftRadius tl
    |> BorderTop.rightRadius tr
    |> BorderBottom.leftRadius bl
    |> BorderBottom.rightRadius br
