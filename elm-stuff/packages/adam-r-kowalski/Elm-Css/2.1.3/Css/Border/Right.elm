module Css.Border.Right where

{-| This library allows you to set the right border of any html element.
You can set the color, style, and width.

# Functions
@docs color, style, width
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles, colorString, px)
import Css.Border.Style as BorderStyle exposing (Style)


{-| Set the color of the right border

    -- Set the right border color to red
    import Css.Border.Right as BorderRight

    -- [ ("border-right-color", "rgba(255, 0, 0, 1)") ]
    BorderRight.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "border-right-color" (colorString c) styles


{-| Set the style of the right border

    -- Set the right border style to dashed
    import Css.Border.Style as BorderStyle
    import Css.Border.Right as BorderRight

    -- [ ("border-right-style, "dashed") ]
    BorderRight.style BorderStyle.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "border-right-style" (BorderStyle.string s) styles


{-| Set the width of the right border

    -- Set the right border width to 5 pixels
    import Css.Border.Right as BorderRight

    -- [ ("border-right-width, "5px") ]
    BorderRight.width 5 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "border-right-width" (px w) styles
