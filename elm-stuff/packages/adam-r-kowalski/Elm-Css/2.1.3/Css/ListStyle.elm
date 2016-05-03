module Css.ListStyle where

{-| ListStyle allows you to set different list item markers including images.

#Definitions
@docs Position, BulletType

#String
@docs positionString, bulletTypeString

#Functions
@docs image, position, bulletType
-}

-- My Imports
import Css exposing (Styles, url)


{-| Represent if the list item markers should appear inside
or outside the content flow.

Inside -
  Indents the marker and the text.  The bullets appear inside the content flow.

Outside -
  Keeps the marker to the left of the text.  The bullets appear outside
  the content flow.
-}
type Position
    = Inside
    | Outside


{-| Represent the bullet type of list item marker in a list.

Disc -
  The marker is a filled circle.

Armenian -
  The marker is traditional Armenian numbering.

Circle -
  The marker is a circle.

CjkIdeographic -
  The marker is plain ideographic numbers.

Decimal -
  The marker is a number.

DecimalLeadingZero -
  The marker is a number with leading zeros
  (01, 02, 03, etc.)

Georgian -
  The marker is traditional Georgian numbering.

Hebrew -
  The marker is traditional Hebrew numbering.

Hiragana -
  The marker is traditional Hiragana numbering.

HiraganaIroha -
  The marker is traditional Hiragana iroha numbering.

Katakana -
  The marker is traditional Katakana numbering.

KatakanaIroha -
  The marker is traditional Katakana iroha numbering.

LowerAlpha -
  The marker is lower-alpha (a, b, c, d, e, etc.)

LowerGreek -
  The marker is lower-greek.

LowerLatin -
  The marker is lower-latin (a, b, c, d, e, etc.)

LowerRoman -
  The marker is lower-roman (i, ii, iii, iv, v, etc.)

None -
  No marker is shown.

Square -
  The marker is a square.

UpperAlpha -
  The marker is upper-alpha (A, B, C, D, E, etc.)

UpperLatin -
  The marker is upper-latin (A, B, C, D, E, etc.)

UpperRoman -
  The marker is upper-roman (I, II, III, IV, V, etc.)
-}
type BulletType
    = Disc
    | Armenian
    | Circle
    | CjkIdeographic
    | Decimal
    | DecimalLeadingZero
    | Georgian
    | Hebrew
    | Hiragana
    | HiraganaIroha
    | Katakana
    | KatakanaIroha
    | LowerAlpha
    | LowerGreek
    | LowerLatin
    | LowerRoman
    | None
    | Square
    | UpperAlpha
    | UpperLatin
    | UpperRoman


{-| Convert a position type to string form so it works in css.

    import Css.ListStyle as ListStyle

    ListStye.positionString ListStyle.Inside -- "inside"
-}
positionString : Position -> String
positionString position =
  case position of
    Inside ->
        "inside"

    Outside ->
        "outside"


{-| Convert a bullet type to string form so it works in css.

    import Css.ListStyle as ListStyle

    ListStyle.typeString ListStyle.Disc -- "disc"
-}
bulletTypeString : BulletType -> String
bulletTypeString t =
  case t of
    Disc ->
        "disc"

    Armenian ->
        "armenian"

    Circle ->
        "circle"

    CjkIdeographic ->
        "cjk-ideographic"

    Decimal ->
        "decimal"

    DecimalLeadingZero ->
        "decimal-leading-zero"

    Georgian ->
        "georgian"

    Hebrew ->
        "hebrew"

    Hiragana ->
        "hiragana"

    HiraganaIroha ->
        "hiragana-iroha"

    Katakana ->
        "katakana"

    KatakanaIroha ->
        "katakana-iroha"

    LowerAlpha ->
        "lower-alpha"

    LowerGreek ->
        "lower-greek"

    LowerLatin ->
        "lower-latin"

    LowerRoman ->
        "lower-roman"

    None ->
        "none"

    Square ->
        "square"

    UpperAlpha ->
        "upper-alpha"

    UpperLatin ->
        "upper-latin"

    UpperRoman ->
        "upper-roman"


{-|

    import Css.ListStyle as ListStyle

    -- [ ("list-style-image", "url(\"smiley.gif\")") ]
    ListStyle.image "smiley.gif" []
-}
image : String -> Styles -> Styles
image path styles =
  Css.style "list-style-image" (url path) styles

{-|

    import Css.ListStyle as ListStyle

    -- [ ("list-style-position", "inside") ]
    ListStyle.position ListStyle.Inside []
-}
position : Position -> Styles -> Styles
position p styles =
  Css.style "list-style-position" (positionString p) styles


{-|

    import Css.ListStyle as ListStyle

    -- [ ("list-style-type", "katakana") ]
    ListStyle.bulletType ListStyle.Katakana []
-}
bulletType : BulletType -> Styles -> Styles
bulletType t styles =
  Css.style "list-style-type" (bulletTypeString t) styles
