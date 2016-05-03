module Css.Border.Left where

{-| This library allows you to set the left border of any html element.
You can set the color, style, and width.

# Functions
@docs color, style, width
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles, colorString, px)
import Css.Border.Style as BorderStyle exposing (Style)


{-| Set the color of the left border

    -- Set the left border color to red
    import Css.Border.Left as BorderLeft


    -- [ ("border-left-color", "rgba(255, 0, 0, 1)") ]
    BorderLeft.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "border-left-color" (colorString c) styles


{-| Set the style of the left border

    -- Set the left border style to dashed
    import Css.Border.Style as BorderStyle
    import Css.Border.Left as BorderLeft


    -- [ ("border-left-style, "dashed") ]
    BorderLeft.style BorderStyle.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "border-left-style" (BorderStyle.string s) styles


{-| Set the width of the left border

    -- Set the left border width to 5 pixels
    import Css.Border.Left as BorderLeft


    -- [ ("border-left-width, "5px") ]
    BorderLeft.width 5 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "border-left-width" (px w) styles
