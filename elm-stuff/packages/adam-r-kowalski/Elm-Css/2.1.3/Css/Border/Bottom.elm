module Css.Border.Bottom where

{-| This library allows you to set the bottom border of any html element.
You can set the color, style, width, and radius of the bottom border.

# Functions
@docs color, style, width, rightRadius, leftRadius
-}

-- Native Imports
import Color exposing (Color)

-- My Imports
import Css exposing (Styles, colorString, px)
import Css.Border.Style as BorderStyle exposing (Style)


{-| Sets the color of the bottom border

    -- Sets the bottom border color to red
    import Css.Border.Bottom as BorderBottom


    -- [ ("border-bottom-color", "rgba(255, 0, 0, 1)") ]
    BorderBottom.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "border-bottom-color" (colorString c) styles


{-| Sets the style of the bottom border

    -- Sets the bottom border style to dashed
    import Css.Border.Style as BorderStyle
    import Css.Border.Bottom as BorderBottom


    -- [ ("border-bottom-style, "dashed") ]
    BorderBottom.style BorderStyle.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "border-bottom-style" (BorderStyle.string s) styles


{-| Sets the width of the bottom border

    -- Sets the bottom border width to 5 pixels
    import Css.Border.Bottom as BorderBottom

    -- [ ("border-bottom-width, "5px") ]
    BorderBottom.width 5 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "border-bottom-width" (px w) styles


{-| Set the bottom right border radius

    -- Set the bottom right border radius to 25 pixels
    import Css.Border.Bottom as BorderBottom


    -- [ ("border-bottom-right-radius, "25px") ]
    BorderBottom.rightRadius 25 []
-}
rightRadius : number -> Styles -> Styles
rightRadius r styles =
  Css.style "border-bottom-right-radius" (px r) styles


{-| Set the bottom left border radius
    -- Set the bottom left border radius to 25 pixels
    import Css.Border.Bottom as BorderBottom

    -- [ ("border-bottom-left-radius, "25px") ]
    BorderBottom.leftRadius 25 []
-}
leftRadius : number -> Styles -> Styles
leftRadius l styles =
  Css.style "border-bottom-left-radius" (px l) styles
