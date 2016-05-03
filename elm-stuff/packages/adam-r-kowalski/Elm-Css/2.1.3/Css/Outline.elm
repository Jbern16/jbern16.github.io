module Css.Outline where

{-| An outline is a line that is drawn around elements (outside the borders)
to make the element "stand out".

#Definition
@docs Style

#String
@docs styleString

#Funcitons
@docs color, style, width, all
-}


-- Native Imports
import Color exposing (Color)

-- My Imports
import Css exposing (Styles, colorString, px)


{-| Represent the style of the outline

None -
  Specifies no outline.

Hidden -
  Specifies a hidden outline.

Dotted -
  Specifies a dotted outline.

Dashed -
  Specifies a dashed outline.

Solid -
  Specifies a solid outline.

Double -
  Specifies a double outliner.

Groove -
  Specifies a 3D grooved outline.
  The effect depends on the outline-color value.

Ridge -
  Specifies a 3D ridged outline.
  The effect depends on the outline-color value.

Inset -
  Specifies a 3D inset outline.
  The effect depends on the outline-color value.

Outset -
  Specifies a 3D outset outline.
  The effect depends on the outline-color value.
-}
type Style
    = None
    | Hidden
    | Dotted
    | Dashed
    | Solid
    | Double
    | Groove
    | Ridge
    | Inset
    | Outset


{-| Convert a style type to string form so it works in css.

    import Css.Outline as Outline

    Outline.styleString Outline.Groove -- "groove"
-}
styleString : Style -> String
styleString style =
  case style of
    None ->
        "none"

    Hidden ->
        "hidden"

    Dotted ->
        "dotted"

    Dashed ->
        "dashed"

    Solid ->
        "solid"

    Double ->
        "double"

    Groove ->
        "groove"

    Ridge ->
        "ridge"

    Inset ->
        "inset"

    Outset ->
        "outset"


{-| Set the color of the outline.

    import Color exposing (rgba)
    import Css.Outline as Outline

    -- [ ("outline-color", "rgba(255, 0, 0, 1)") ]
    Outline.color (rgba 255 0 0 1) []

-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "outline-color" (colorString c) styles


{-| Set the style of the outline.

    import Css.Outline as Outline

    -- [ ("outline-style", "dashed") ]
    Outline.style Outline.Dashed []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "outline-style" (styleString s) styles


{-| Set the width of the outline.

    import Css.Outline as Outline

    -- [ ("outline-width", "2px") ]
    Outline.width 2 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "outline-width" (px w) styles


{-| Set all of the outline properties in one function

    import Css.Outline as Outline

    {--
      [ ("outline-color", "rgba(255, 0, 0, 1)")
      , ("outline-style", "dashed")
      , ("outline-width", "2px")
      ]
    --}
    Outline.all (rgba 255 0 0 1) Outline.Dashed 2 []
-}
all : Color -> Style -> number -> Styles -> Styles
all c s w styles =
  styles
    |> color c
    |> style s
    |> width w
