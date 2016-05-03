module Css.Font where

{-| Font properties define the font family, boldness, size,
and the style of a text.

Serif -
  Serif fonts have small lines at the ends on some characters.

Sans-Serif -
  "Sans" means without - these fonts do not have the lines at
  the ends of characters.

Monospace -
  All monospace characters have the same width.

#Definition
@docs Style, Variant

#Strings
@docs styleString, variantString

#Functions
@docs family, size, style, variant, weight
-}

-- My Imports
import Css exposing (Styles, px)


{-| Represent the style of the font.

NormalStyle -
  The text is shown normally.

Italic -
  The text is shown in italics.

Oblique -
  The text is "leaning" (oblique is very similar to
  italic, but less supported).
-}
type Style
    = NormalStyle
    | Italic
    | Oblique


{-| Represent whether the font is small caps or normal.

Normal -
  The browser displays a normal font.

SmallCaps -
  The browser displays a small-caps font.
-}
type Variant
  = NormalVariant
  | SmallCaps


{-| Convert a style type to string form so it works in css.

    import Css.Font as Font

    Font.styleString Font.Italic -- "italic"
-}
styleString : Style -> String
styleString style =
  case style of
    NormalStyle ->
        "normal"

    Italic ->
        "italic"

    Oblique ->
        "oblique"


{-| Convert a variant type to string form so it works in css.

    import Css.Font as Font

    Font.variantString Font.SmallCaps -- "small-caps"
-}
variantString : Variant -> String
variantString v =
  case v of
    NormalVariant ->
      "normal"

    SmallCaps ->
      "small-caps"


{-| Set the font family for text.

    import Css.Font as Font

    -- [ ("font-family", "\"Times New Roman\", Georgia, Serif") ]
    Font.family "\"Times New Roman\", Georgia, Serif" []
-}
family : String -> Styles -> Styles
family f styles =
  Css.style "font-family" (f) styles


{-| Set the font size of text.

    import Css.Font as Font

    -- [ ("font-size", "24px") ]
    Font.size 24 []
-}
size : number -> Styles -> Styles
size length styles =
  Css.style "font-size" (px length) styles


{-| Set the font style for text.

    import Css.Font as Font

    -- [ ("font-style", "italic") ]
    Font.style Font.Italic []
-}
style : Style -> Styles -> Styles
style s styles =
  Css.style "font-style" (styleString s) styles


{-| Set whether or not a text should be displayed in a small-caps font.

    import Css.Font as Font

    -- [ ("font-variant", "small-caps") ]
    Font.variant Font.SmallCaps []
-}
variant : Variant -> Styles -> Styles
variant v styles =
  Css.style "font-variant" (variantString v) styles


{-| Set the weight of the font. (Only values 1-9 have any significance)

    import Css.Font as Font

    -- [ ("font-weight", "900") ]
    Font.weight 9 []
-}
weight : Int -> Styles -> Styles
weight w styles =
  let ub = if w > 9 then 9 else w
      lb = if ub < 1 then 1 else ub
  in
    Css.style "font-weight" (toString (lb * 100)) styles
