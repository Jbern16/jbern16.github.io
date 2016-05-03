module Css.Transform2 where

{-| Transforms allow you to translate, rotate, scale, and skew elements.
A transformation is an effect that lets an element change shape, size,
and position.

#Functions
@docs transform2, translate2, rotate2, scale2, skew2, matrix2

-}

-- Native Imports
import String

-- Third Party Imports
import Vendor

-- My Imports
import Css exposing (Styles, style)


{-| This function takes a list of transforms and applies them
to the element.

    import Css.Transform2 exposing (..)

    -- [ ("transform", "translate(100px, 100px) rotate(45deg)") ]
    transform2 [ translate2 100 100, rotate2 45 ] []
-}
transform2 : List String -> Styles -> Styles
transform2 transforms styles =
  let string = String.join " " transforms
      prefix = Vendor.prefix
      name = "transform"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) string styles

    else if prefix == Vendor.MS
    then style ("-ms-" ++ name) string styles

    else style name string styles


{-| Moves an element from its current position (according to the
parameters given for the X-axis and the Y-axis).

    import Css.Transform2 exposing (..)

    -- [ ("transform", "translate(100px, 100px)") ]
    transform2 [ translate2 100 100 ] []
-}
translate2 : number -> number -> String
translate2 x y =
  "translate(" ++ (toString x) ++ "px, " ++ (toString y) ++ "px)"


{-| Rotates the element clockwise or counter-clockwise according
to the given degree.

    import Css.Transform2 exposing (..)

    -- [ ("transform", "rotate(45deg)") ]
    transform2 [ rotate2 45 ] []
-}
rotate2 : number -> String
rotate2 degrees =
  "rotate(" ++ (toString degrees) ++ "deg)"


{-| Increases or decreases the size of an element (according
to the parameters given for the width and height).

    import Css.Transform2 exposing (..)

    -- [ ("transform", "scale(0.5, 0.5)") ]
    transform2 [ scale2 0.5 0.5 ] []
-}
scale2 : number -> number -> String
scale2 x y =
  "scale(" ++ (toString x) ++ "," ++ (toString y) ++ ")"


{-| Skews an element along the X and Y-axis by the given angles.

    import Css.Transform2 exposing (..)

    -- [ ("transform", "skew(20deg, 20deg)") ]
    transform2 [ skew2 20 20 ] []
-}
skew2 : number -> number -> String
skew2 x y =
  "skew(" ++ (toString x) ++ "deg, " ++ (toString y) ++ "deg)"


{-| Combines all the 2D transform methods into one.

The matrix method takes six parameters, containing mathematic functions,
which allow you to rotate, scale, move (translate), and skew elements.

The default matrix has the following form

1 0 0

1 0 0

0 0 1

This means that the element has a scale of 1 in the x and y direction,
as well as no skew and no translation.

The browser multiplies this by a vector which represents a particular
point within the local coordinate space.

x

y

1

If you look at the parameters that this function takes you may also notice
that it doesn't take a rotation. So how do we accomplish this? By placing
opposite values into the x and y skew you will have a rotation.

1 -0.5 0.5

1   0   0

0   0   1

We only need to specify the first six parameters as the last three will always
be 0 0 1.

scalex skewx skewy

scaley   x     y

  0      0     1

    import Css.Transform2 exposing (..)

    -- [ ("transform", "matrix(1, 0, 0, 1, 0, 0)") ]
    transform2 [ matrix2 1 0 0 1 0 0 ] []
-}
matrix2 : number -> number -> number -> number -> number -> number -> String
matrix2 scalex skewx skewy scaley x y =
  "matrix(" ++ (toString scalex) ++ ", " ++ (toString skewx) ++ ", " ++
  (toString skewy) ++ ", " ++ (toString scaley) ++ ", " ++
  (toString x) ++ ", " ++ (toString y) ++ ")"
