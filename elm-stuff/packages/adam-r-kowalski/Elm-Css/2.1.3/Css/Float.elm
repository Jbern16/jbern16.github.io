module Css.Float where

{-| Float can be used to push elements left or right,
allowing other elements to wrap around it.

#Definitions
@docs Clear, FloatType

#Strings
@docs clearString, floatString

#Functions
@docs clear, float
-}

-- My Imports
import Css exposing (Styles)


{-| Represent the sides of an element where
floating elements are not allowed to float.

ClearLeft -
  No floating elements allowed on the left side.

ClearRight -
  No floating elements allowed on the right side.

ClearBoth -
  No floating elements allowed on either the left or right side.

ClearNone -
  Allows floating elements on both sides.
-}
type Clear
    = ClearLeft
    | ClearRight
    | ClearBoth
    | ClearNone


{-| Represent whether or not an element should float.

Left -
  The element floats to the left.

Right -
  The element floats to the right.

None -
  The element is not floated, and will be displayed
  just where it occurs in the text.
-}
type FloatType
    = Left
    | Right
    | None


{-| Convert a clear type to string form so it works in css.

    import Css.Float as CssFloat

    CssFloat.clearString CssFloat.ClearBoth -- "both"
-}
clearString : Clear -> String
clearString clear =
  case clear of
    ClearLeft ->
        "left"

    ClearRight ->
        "right"

    ClearBoth ->
        "both"

    ClearNone ->
        "none"


{-| Convert a float type to string form so it works in css.

    import Css.Float as CssFloat

    CssFloat.floatString CssFloat.Left -- "left"
-}
floatString : FloatType -> String
floatString float =
  case float of
    Left ->
        "left"

    Right ->
        "right"

    None ->
        "none"


{-| Set the sides of an element where
floating elements are not allowed to float.

    import Css.Float as CssFloat

    -- [ ("clear", "left") ]
    CssFloat.clear CssFloat.ClearLeft []
-}
clear : Clear -> Styles -> Styles
clear c styles =
  Css.style "clear" (clearString c) styles


{-| Set whether or not an element should float.

    import Css.Float as CssFloat exposing (float)

    -- [ ("float", "left") ]
    float CssFloat.Left []
-}
float : FloatType -> Styles -> Styles
float f styles =
  Css.style "float" (floatString f) styles
