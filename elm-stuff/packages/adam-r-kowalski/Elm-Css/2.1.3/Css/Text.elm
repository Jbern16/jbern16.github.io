module Css.Text where

{-| Change the way that text is displayed on the screen.

#Definitions
@docs Direction, Align, Decoration, UnicodeBidi, WhiteSpace

#Strings
@docs directionString, alignString, decorationString, unicodeBidiString, whiteSpaceString

#Functions
@docs color, direction, letterSpacing, lineHeight, align, decoration, indent, unicodeBidi, whiteSpace, wordSpacing
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles, colorString, px)


{-| Represent the text direction/writing direction.

Ltr -
  The writing direction is left-to-right.

Rtl -
  The writing direction is right-to-left.
-}
type Direction
    = Ltr
    | Rtl


{-| Specifies the horizontal alignment of text.

Left -
  Aligns the text to the left.

Right -
  Aligns the text to the right.

Center -
  Centers the text.

Justify -
  Stretches the lines so that each line has equal width
  (like in newspapers and magazines).
-}
type Align
    = Left
    | Right
    | Center
    | Justify


{-| Represent the decoration added to text.

NoDecoration -
  Defines a normal text.

Underline -
  Defines a line below the text.

Overline -
  Defines a line above the text.

LineThrough -
  Defines a line through the text.
-}
type Decoration
    = NoDecoration
    | Underline
    | Overline
    | LineThrough


{-| The unicode-bidi property is used together with the direction property to
set or return whether the text should be overridden to support multiple
languages in the same document.

NormalUnicodeBidi -
  - Does not use an additional level of embedding.

Embed -
  Creates an additional level of embedding.

BidiOverride -
  Creates an additional level of embedding. Reordering depends on the direction
  property.
-}
type UnicodeBidi
    = NormalUnicodeBidi
    | Embed
    | BidiOverride


{-| Represent how white space inside an element is handled.

NormalWhiteSpace -
  Sequences of whitespace will collapse into a single whitespace. Text will
  wrap when necessary.

NoWrap -
  Sequences of whitespace will collapse into a single whitespace.  Text will
  never wrap to the next line. The text continues on the same line until
  a <br> tag is encountered.

Pre -
  Whitespace is preserved by the browser. Text will only wrap on line breaks.
  Acts like the <pre> tag in HTML.

PreLine -
  Sequences of whitespace will collapse into a single whitespace.
  Text will wrap when necessary, and on line breaks.

PreWrap -
  Whitespace is preserved by the browser. Text will wrap when necessary,
  and on line breaks.
-}
type WhiteSpace
    = NormalWhiteSpace
    | NoWrap
    | Pre
    | PreLine
    | PreWrap


{-| Convert a direction type to string form so it works with css.

    import Css.Text as Text

    Text.directionString Text.Ltr -- "ltr"
-}
directionString : Direction -> String
directionString direction =
  case direction of
    Ltr ->
        "ltr"

    Rtl ->
        "Rtl"


{-| Convert an align type to string form so it works with css.

    import Css.Text as Text

    Text.alignString Text.Left -- "left"
-}
alignString : Align -> String
alignString align =
  case align of
    Left ->
        "left"

    Right ->
        "right"

    Center ->
        "center"

    Justify ->
        "justify"


{-| Convert an decoration type to string form so it works with css.

    import Css.Text as Text

    Text.decorationString Text.Underline -- "underline"
-}
decorationString : Decoration -> String
decorationString decoration =
  case decoration of
    NoDecoration ->
        "none"

    Underline ->
        "underline"

    Overline ->
        "overline"

    LineThrough ->
        "line-through"


{-| Convert an unicode bidi type to string form so it works with css.

    import Css.Text as Text

    Text.unicodeBidiString Text.Embed -- "embed"
-}
unicodeBidiString : UnicodeBidi -> String
unicodeBidiString unicodeBidi =
  case unicodeBidi of
    NormalUnicodeBidi ->
        "normal"

    Embed ->
        "embed"

    BidiOverride ->
        "bidi-override"


{-| Convert an white space type to string form so it works with css.

    import Css.Text as Text

    Text.whiteSpaceString Text.Pre -- "pre"
-}
whiteSpaceString : WhiteSpace -> String
whiteSpaceString whiteSpace = case whiteSpace of
    NormalWhiteSpace ->
        "normal"

    NoWrap ->
        "nowrap"

    Pre ->
        "pre"

    PreLine ->
        "pre-line"

    PreWrap ->
        "pre-wrap"


{-| Set the color of text.

    import Css.Text as Text

    -- [ ("color", "rgba(255, 0, 0, 1)") ]
    Text.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "color" (colorString c) styles


{-| Set the text direction/writing direction.

    import Css.Text as Text

    -- [ ("direction", "ltr") ]
    Text.direction Text.Ltr []
-}
direction : Direction -> Styles -> Styles
direction d styles =
  Css.style "direction" (directionString d) styles


{-| Increases or decreases the space between characters in a text.

    import Css.Text as Text

    -- [ ("letter-spacing", "2px") ]
    Text.letterSpacing 2 []
-}
letterSpacing : number -> Styles -> Styles
letterSpacing length styles =
  Css.style "letter-spacing" (px length) styles


{-| Set the line height.

    import Css.Text as Text

    -- [ ("line-height", "14px") ]
    Text.lineHeight 14 []
-}
lineHeight : number -> Styles -> Styles
lineHeight length styles =
  Css.style "line-height" (px length) styles


{-| Set the horizontal alignment of text.

    import Css.Text as Text

    -- [ ("text-align", "left") ]
    Text.align Text.Left []
-}
align : Align -> Styles -> Styles
align a styles =
  Css.style "text-align" (alignString a) styles


{-| Set the decoration added to text.

    import Css.Text as Text

    -- [ ("text-decoration", "underline") ]
    Text.decoration Text.Underline []
-}
decoration : Decoration -> Styles -> Styles
decoration d styles =
  Css.style "text-decoration" (decorationString d) styles


{-| Set the indentation of the first line in a text-block.

    import Css.Text as Text

    -- [ ("text-indent", "20px") ]
    Text.indent 20 []
-}
indent : number -> Styles -> Styles
indent length styles =
  Css.style "text-indent" (px length) styles


{-| Used together with the direction property to set or return whether
the text should be overridden to support multiple languages
in the same document.

    import Css.Text as Text

    -- [ ("unicode-bidi", "bidi-override") ]
    Text.unicodeBidi Text.BidiOverride []
-}
unicodeBidi : UnicodeBidi -> Styles -> Styles
unicodeBidi u styles =
  Css.style "unicode-bidi" (unicodeBidiString u) styles


{-| Set how white space inside an element is handled.

    import Css.Text as Text

    -- [ ("white-space", "pre") ]
    Text.whiteSpace Text.Pre []
-}
whiteSpace : WhiteSpace -> Styles -> Styles
whiteSpace ws styles =
  Css.style "white-space" (whiteSpaceString ws) styles


{-| Increases or decreases the space between words in a text.

    import Css.Text as Text

    -- [ ("word-spacing", "2px") ]
    Text.wordSpacing 2 []
-}
wordSpacing : number -> Styles -> Styles
wordSpacing length styles =
  Css.style "word-spacing" (px length) styles
