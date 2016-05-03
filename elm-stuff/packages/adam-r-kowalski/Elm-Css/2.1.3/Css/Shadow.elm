module Css.Shadow where

{-| With CSS3 you can add shadow to text and to elements.

#Definitions
@docs Text, Box

#Strings
@docs textString, listTextString, boxString, listBoxString

#Functions
@docs text, box
-}

-- Native Imports
import Color exposing (Color)

-- My Imports
import Css exposing (Styles, colorString, px)

{-| Represent a text shadow.

Int -
  The position of the horizontal shadow.
  Negative values are allowed.

Int -
  The position of the vertical shadow.
  Negative values are allowed.

Int -
  The blur radius.

Color -
  The color of the shadow.
-}
type alias Text =
    (Int, Int, Int, Color)


{-| Represent a box shadow.

Int -
  The position of the horizontal shadow.
  Negative values are allowed.

Int -
  The position of the vertical shadow.
  Negative values are allowed.

Int -
  The blur radius.

Int -
  The size of shadow. Negative values are allowed.

Color -
  The color of the shadow.

Bool -
  Changes the shadow from an outer shadow
  (outset) to an inner shadow.
-}
type alias Box =
    (Int, Int, Int, Int, Color, Bool)


{-| Convert a text shadow to string form so it works with css.

    import Css.Shadow as Shadow

    -- "10px 10px 10px rgba(0, 0, 0, 1)"
    Shadow.textString (10, 10, 10, (rgba 0 0 0 1))
-}
textString : Text -> String
textString (horizontal, vertical, blur, color) =
  (px horizontal) ++ (px vertical) ++ (px blur) ++ (colorString color)


{-| Convert a list of text shadows to string form so it works with css.

    import Css.Shadow as Shadow

    -- [ ( "text-shadow" , "10px 10px 10px rgba(0, 0, 0, 1) ]
    Shadow.listTextString [ (10, 10, 10, (rgba 0 0 0 1)) ]
-}
listTextString : String -> List Text -> String
listTextString accumulator textShadows =
  case textShadows of
    [] -> accumulator
    head::[] -> listTextString (accumulator ++ (textString head)) []
    head::tail -> listTextString (accumulator ++ (textString head) ++ ", ") tail


{-| Convert a box shadow to string form so it works with css.

    import Css.Shadow as Shadow

    -- "10px 10px 10px 5px rgba(0, 0, 0, 1)"
    shadow.boxString (10, 10, 10, 5, (rgba 0 0 0 1), false)
-}
boxString : Box -> String
boxString (horizontal, vertical, blur, spread, color, inset) =
  let h = px horizontal
      v = px vertical
      b = px blur
      s = px spread
      c = colorString color
      i = if inset then " inset" else ""
  in
    h ++ v ++ b ++ s ++ c ++ i


{-| Convert a list of box shadows to string form so it works with css.

    import Css.Shadow as Shadow

    -- [ ( "box-shadow" , "10px 10px 10px rgba(0, 0, 0, 1)") ]
    Shadow.listTextString [ (10, 10, 10, (rgba 0 0 0 1)) ]
-}
listBoxString : String -> List Box -> String
listBoxString accumulator boxShadows =
  case boxShadows of
    [] -> accumulator
    head::[] -> listBoxString (accumulator ++ (boxString head)) []
    head::tail -> listBoxString (accumulator ++ (boxString head) ++ ", ") tail


{-| Attach a shadow to an element.

    import Css.Shadow as Shadow

    -- [ ("box-shadow", "10px 10px 10px 5px rgba(0, 0, 0, 1)") ]
    Shadow.box [ (10, 10, 10, 5, (rgba 0 0 0 1), false ] []
-}
box : List Box -> Styles -> Styles
box boxShadows styles =
  Css.style "box-shadow" (listBoxString "" boxShadows) styles


{-| Set the shadow effect added to text.

    import Css.Shadow as Shadow

    -- [ ("text-shadow", "10px 10px 10px rgba(0, 0, 0, 1)") ]
    Shadow.text [ (10, 10, 10, rgba 0 0 0 1) ] []
-}
text : List Text -> Styles -> Styles
text textShadows styles =
  Css.style "text-shadow" (listTextString "" textShadows) styles
