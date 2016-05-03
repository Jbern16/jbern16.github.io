module Css.Border.Top where

{-| This library allows you to set the top border of any html element.
You can set the color, style, width, and radius of the top border.

# Functions
@docs color, style, width, rightRadius, leftRadius
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles, colorString, px)
import Css.Border.Style as BorderStyle exposing (Style)


{-| Set the color of the top border

    -- Set the top border color to red
    import Css.Border.Top as BorderTop

    -- [ ("border-top-color", "rgba(255, 0, 0, 1)") ]
    BorderTop.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "border-top-color" (colorString c) styles


{-| Set the style of the top border

    -- Set the top border style to dashed
    import Css.Border.Style as BorderStyle
    import Css.Border.Top as BorderTop

    -- [ ("border-top-style, "dashed") ]
    BorderTop.style BorderStyle.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "border-top-style" (BorderStyle.string s) styles


{-| Set the width of the top border

    -- Set the top border width to 5 pixels
    import Css.Border.Top as BorderTop

    -- [ ("border-top-width, "5px") ]
    BorderTop.width 5 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "border-top-width" (px w) styles


{-| Set the top right border radius

    -- Set the top right border radius to 25 pixels
    import Css.Border.Top as BorderTop


    -- [ ("border-top-right-radius, "25px") ]
    BorderTop.rightRadius 25 []
-}
rightRadius : number -> Styles -> Styles
rightRadius r styles =
  Css.style "border-top-right-radius" (px r) styles


{-| Set the top left border radius
    -- Set the top left border radius to 25 pixels
    import Css.Border.Top as BorderTop

    -- [ ("border-top-left-radius, "25px") ]
    BorderTop.leftRadius 25 []
-}
leftRadius : number -> Styles -> Styles
leftRadius l styles =
  Css.style "border-top-left-radius" (px l) styles
