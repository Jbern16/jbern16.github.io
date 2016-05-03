module Css.Transform3 where

{-| Transforms allow you to translate, rotate, scale, and skew elements.
A transformation is an effect that lets an element change shape, size,
and position.

#Functions
@docs transform3, perspective, translate3, rotate3, scale3

-}

-- Native Imports
import String

-- Third Party Imports
import Vendor

-- My Imports
import Css exposing (Styles, style)


{-| This function takes a list of transforms and applies them
to the element.

    import Css.Transform3 exposing (..)

    -- [ ("transform", "translate3(100px, 100px, 0px) rotate3(1, 1, 1, 45deg)") ]
    transform3 [ translate3 100 100 0, rotate1 1 1 1 45 ] []
-}
transform3 : List String -> Styles -> Styles
transform3 transforms styles =
  let string = String.join " " transforms
      prefix = Vendor.prefix
      name = "transform"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) string styles
    else style name string styles


{-| Defines a perspective view for a 3D transformed element.

  -- [ ("transform", "perspective(800)") ]
  transform3 [ perspective 800 ]
-}
perspective : number -> String
perspective p =
  "perspective(" ++ (toString p) ++ ")"


{-| Moves an element from its current position (according to the
parameters given for the X-axis and the Y-axis).

    import Css.Transform3 exposing (..)

    -- [ ("transform", "translate3d(100px, 100px, 0px)") ]
    transform3 [ translate3 100 100 0 ] []
-}
translate3 : number -> number -> number -> String
translate3 x y z =
  "translate3d(" ++ (toString x) ++ "px, " ++
  (toString y) ++ "px, " ++ (toString z) ++ "px)"


{-| Rotates the element around a specified axis at a given degree.

    import Css.Transform3 exposing (..)

    -- [ ("transform", "rotate3d(1, 1, 1, 45deg)") ]
    transform3 [ rotate3 1 1 1 45 ] []
-}
rotate3 : number -> number -> number -> number -> String
rotate3 x y z angle =
  "rotate3d(" ++ (toString x) ++ ", " ++
  (toString y) ++ ", " ++ (toString z) ++ ", " ++
  (toString angle) ++ "deg)"


{-| Scale an element along the given axis.

    import Css.Transform3 exposing (..)

    -- [ ("transform", "scale3d(1, 2, 1)") ]
    transform3 [ scale3 1 2 1 ] []
-}
scale3 : number -> number -> number -> String
scale3 x y z =
  "scale3d(" ++ (toString x) ++ ", " ++
  (toString y) ++ ", " ++ (toString z) ++ ")"
