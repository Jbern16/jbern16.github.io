module Css.Gradient where

{-| Gradients let you display smooth transitions between
two or more specified colors.

#Definitions
@docs linear, radial
-}

-- Native Imports
import Color exposing (Color)
import String

-- Third Party Imports
import Vendor

-- My Imports
import Css exposing (Styles, style, colorString)


{-| To create a linear gradient you must define at least two color stops.
Color stops are the colors you want to render smooth transitions among.
You must also set the angle along with the gradient effect.

    import Css.Gradient as Gradient

    -- [ ("background", "linear-gradient(90deg, rgba(255, 0, 0, 1), rgba(0, 255, 0, 1))") ]
    Gradient.linear 90 [ rgba 255 0 0 1, rgba 0 255 0 1 ] []
-}
linear : number -> List Color -> Styles -> Styles
linear degrees colors styles =
  let colorStrings = String.join "," (List.map colorString colors)
      degreeString = (toString degrees) ++ "deg, "
      string = "linear-gradient(" ++ degreeString ++ colorStrings ++ ")"
      prefix = Vendor.prefix
      name = "background"
  in
    if prefix == Vendor.Webkit
    then style name ("-webkit-" ++ string) styles

    else if prefix == Vendor.Moz
    then style name ("-moz-" ++ string) styles

    else if prefix == Vendor.O
    then style name ("-o-" ++ string) styles

    else style name string styles


{-| A radial gradient is defined by its center. To create a radial gradient
you must also define at least two color stops.

    import Css.Gradient as Gradient

    -- [ ("background", "radial-gradient(rgba(255, 0, 0, 1), rgba(0, 255, 0, 1))") ]
    Gradient.radial [ rgba 255 0 0 1, rgba 0 255 0 1 ] []
-}
radial : List Color -> Styles -> Styles
radial colors styles =
  let colorStrings = String.join "," (List.map colorString colors)
      string = "radial-gradient(" ++ colorStrings ++ ")"
      prefix = Vendor.prefix
      name = "background"
  in
    if prefix == Vendor.Webkit
    then style name ("-webkit-" ++ string) styles

    else if prefix == Vendor.Moz
    then style name ("-moz-" ++ string) styles

    else if prefix == Vendor.O
    then style name ("-o-" ++ string) styles

    else style name string styles
