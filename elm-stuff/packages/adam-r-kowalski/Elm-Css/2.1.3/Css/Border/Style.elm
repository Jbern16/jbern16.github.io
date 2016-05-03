module Css.Border.Style where

{-| This library allows you to set the
style of the border.

# Definition
@docs Style

# String
@docs string
-}

{-| Represent the types of styles that can be
applied to the border.

None -
  Default value. Specifies no border.

Hidden -
  The same as "none", except in border conflict
  resolution for table elements.

Dotted -
  Specifies a dotted border.

Dashed -
  Specifies a dashed border.

Solid -
  Specifies a solid border.

Double -
  Specifies a double border.

Groove -
  Specifies a 3D grooved border.  The effect
  depends on the border-color value.

Ridge -
  Specifies a 3D ridged border.
  The effect depends on the border-color value.

Inset -
  Specifies a 3D inset border.
  The effect depends on the border-color value.

Outset -
  Specifies a 3D outset border.
  The effect depends on the border-color value.
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


{-| Convert a style to the string form so it works with css.

    import Css.Style as Style

    Style.string Style.Groove [] -- "groove"
-}
string : Style -> String
string style =
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
