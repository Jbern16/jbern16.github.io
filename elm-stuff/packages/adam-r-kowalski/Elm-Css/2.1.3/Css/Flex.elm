module Css.Flex where

{-| Set the layout of your elements using the flex layout system.

#Definitions
@docs Direction, Wrap, JustifyContent, AlignItem, AlignContent

#Strings
@docs directionString, wrapString, justifyContentString, alignItemString, alignContentString

#Direction and Wrap
@docs direction, wrap, flow

#Alignment
@docs justifyContent, alignItems, alignContent

#Child Properties
@docs order, grow, shrink, basis, alignSelf
-}

-- Third Party Imports
import Vendor

-- My Imports
import Css exposing (Styles, style, px)

{-| Represent the main-axis, thus defining the direction
flex items are placed in the flex container.

Row -
  Left to right in ltr; right to left in rtl.

RowReverse -
  Right to left in ltr; left to right in rtl.

Column -
  Same as row but top to bottom.

ColumnReverse -
  Same as row-reverse but bottom to top.
-}
type Direction
    = Row
    | RowReverse
    | Column
    | ColumnReverse


{-| Represent the wrap behavior of flex items.

NoWrap -
  Single line / left to right in ltr; right to left in rtl.

Wrap -
  Multi-line / left to right in ltr; right to left in rtl.

WrapReverse -
  Multi-line / right to left in ltr; left to right in rtl.
-}
type Wrap
    = NoWrap
    | Wrap
    | WrapReverse


{-| Represent the alignment along the main axis.

JCStart -
  Items are packed toward the start line.

JCEnd -
  Items are packed toward the end line.

JCCenter -
  Items are centered along the line.

JCBetween -
  Items are evenly distributed in the line;
  first item is on the start line, last item on the end line.

JCAround -
  Items are evenly distributed in the line with
  equal space around them.
-}
type JustifyContent
    = JCStart
    | JCEnd
    | JCCenter
    | JCBetween
    | JCAround


{-| Represent how the flex item is laid out along the
cross axis on the current line.

AIStart -
  Cross start margin edge of the items is placed on the cross start line.

AIEnd -
  Cross end margin edge of the items is placed on the cross end line.

AICenter -
  Items are centered in the cross axis.

AIStretch -
  Stretch to fill the container (still respect min and max width).

AIBaseline -
  Items are aligned such as their baselines align.
-}
type AlignItem
    = AIStart
    | AIEnd
    | AICenter
    | AIStretch
    | AIBaseline


{-| Represent how to align the flex container's lines within when
there is extra space in the cross axis.

ACStart -
  Lines packed to the start of the container.

ACEnd -
  Lines packed to the end of the container.

ACCenter -
  Lines packed to the center of the container.

ACStretch -
  Lines stretch to take up the remaining space.

ACBetween -
  Lines evenly distributed; the first line is at the start of the
  container while the last one is at the end.

ACAround -
  Lines evenly distributed with equal space around each line.
-}
type AlignContent
    = ACStart
    | ACEnd
    | ACCenter
    | ACStretch
    | ACBetween
    | ACAround


{-| Convert a direction type to string form so it works with css.

    import Css.Flex as Flex

    Flex.directionString Flex.Row -- "row"
-}
directionString : Direction -> String
directionString direction =
  case direction of
    Row ->
        "row"

    RowReverse ->
        "row-reverse"

    Column ->
        "column"

    ColumnReverse ->
        "column-reverse"


{-| Convert a wrap type to string form so it works with css.

    import Css.Flex as Flex

    Flex.wrapString Flex.NoWrap -- "nowrap"
-}
wrapString : Wrap -> String
wrapString wrap =
  case wrap of
    NoWrap ->
        "nowrap"

    Wrap ->
        "wrap"

    WrapReverse ->
        "wrap-reverse"


{-| Convert a justify content type to string form so it works with css.

    import Css.Flex as Flex

    Flex.justifyContentString Flex.JCStart -- "flex-start"
-}
justifyContentString : JustifyContent -> String
justifyContentString justify =
  case justify of
    JCStart ->
        "flex-start"

    JCEnd ->
        "flex-end"

    JCCenter ->
        "center"

    JCBetween ->
        "space-between"

    JCAround ->
        "space-around"


{-| Convert a align item type to string form so it works with css.

    import Css.Flex as Flex

    Flex.alignItemString Flex.AIStart -- "flex-start"
-}
alignItemString : AlignItem -> String
alignItemString align =
  case align of
    AIStart ->
        "flex-start"

    AIEnd ->
        "flex-end"

    AICenter ->
        "center"

    AIStretch ->
        "stretch"

    AIBaseline ->
        "baseline"


{-| Convert a align content type to string form so it works with css.

    import Css.Flex as Flex

    Flex.alignContentString Flex.ACStart -- "flex-start"
-}
alignContentString : AlignContent -> String
alignContentString align =
  case align of
    ACStart ->
        "flex-start"

    ACEnd ->
        "flex-end"

    ACCenter ->
        "center"

    ACStretch ->
        "stretch"

    ACBetween ->
        "space-between"

    ACAround ->
        "space-around"


{-| Set the main-axis, thus defining the direction flex items
are placed in the flex container.

    import Css.Flex as Flex

    -- [ ("flex-direction", "row") ]
    Flex.direction Flex.Row []
-}
direction : Direction -> Styles -> Styles
direction d styles =
  let prefix = Vendor.prefix
      name = "flex-direction"
  in
    case d of
      Row ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "row" styles

        else if prefix == Vendor.MS
        then style ("-ms-" ++ name) "row" styles

        else style name "row" styles

      RowReverse ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "row-reverse" styles

        else if prefix == Vendor.MS
        then style ("-ms-" ++ name) "row-reverse" styles

        else style name "row-reverse" styles

      Column ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "column" styles

        else if prefix == Vendor.MS
        then style ("-ms-" ++ name) "column" styles

        else style name "column" styles

      ColumnReverse ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "column-reverse" styles

        else if prefix == Vendor.MS
        then style ("-ms-" ++ name) "column-reverse" styles

        else style name "column-reverse" styles


{-| Set the wrap behavior of flex items.

    import Css.Flex as Flex

    -- [ ("flex-wrap", "nowrap") ]
    Flex.wrap Flex.NoWrap []
-}
wrap : Wrap -> Styles -> Styles
wrap w styles =
  let prefix = Vendor.prefix
      name = "flex-wrap"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) (wrapString w) styles

    else if prefix == Vendor.MS
    then style ("-ms-" ++ name) (wrapString w) styles

    else style name (wrapString w) styles


{-| Set the direction and the wrap together.

    import Css.Flex as Flex

    -- [ ("flex-direction", "row")
       , ("flex-wrap", "nowrap")
       ]
    Flex.flow Flex.Row Flex.NoWrap []
-}
flow : Direction -> Wrap -> Styles -> Styles
flow d w styles =
  styles
    |> direction d
    |> wrap w


{-| Set the alignment along the main axis.

    import Css.Flex as Flex

    -- [ ("justify-content", "flex-start") ]
    Flex.justifyContent Flex.JCStart []
-}
justifyContent : JustifyContent -> Styles -> Styles
justifyContent j styles =
  let prefix = Vendor.prefix
      name = "justify-content"
  in
    case j of
      JCStart ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-start" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-pack" "start" styles

        else style name "flex-start" styles

      JCEnd ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-end" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-pack" "end" styles

        else style name "flex-end" styles

      JCCenter ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "center" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-pack" "center" styles

        else style name "center" styles

      JCBetween ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "space-between" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-pack" "justify" styles

        else style name "space-between" styles

      JCAround ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "space-around" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-pack" "distribute" styles

        else style name "space-around" styles


{-| else if Set how the flex items are laid out along the
cross axis on the current line. Think of it as the justifyContent version
for the cross axis (perpendicular to the main axis).

    import Css.Flex as Flex

    -- [ ("align-items", "flex-start") ]
    Flex.alignItems Flex.AIStart []
-}
alignItems : AlignItem -> Styles -> Styles
alignItems a styles =
  let prefix = Vendor.prefix
  in
    case a of
      AIStart ->
        if prefix == Vendor.Webkit
        then style "-webkit-align-items" "flex-start" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-align" "start" styles

        else style "align-items" (alignItemString a) styles

      AIEnd ->
        if prefix == Vendor.Webkit
        then style "-webkit-align-items" "flex-end" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-align" "end" styles

        else style "align-items" (alignItemString a) styles

      _ ->
        if prefix == Vendor.Webkit
        then style "-webkit-align-items" (alignItemString a) styles

        else if prefix == Vendor.MS
        then style "-ms-flex-align" (alignItemString a) styles

        else style "align-items" (alignItemString a) styles


{-| else if Set how to align the flex container's lines within when
there is extra space in the cross axis.

    import Css.Flex as Flex

    -- [ ("align-content", "flex-start") ]
    Flex.alignContent Flex.ACStart []
-}
alignContent : AlignContent -> Styles -> Styles
alignContent a styles =
  let prefix = Vendor.prefix
      name = "align-content"
  in
    case a of
      ACStart ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-start" styles

        else if prefix == Vendor.MS
        then style ("-ms-flex-line-pack") "start" styles

        else style name "flex-start" styles

      ACEnd ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-end" styles

        else if prefix == Vendor.MS
        then style ("-ms-flex-line-pack") "end" styles

        else style name "flex-end" styles

      ACBetween ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "space-between" styles

        else if prefix == Vendor.MS
        then style ("-ms-flex-line-pack") "justify" styles

        else style name "space-between" styles

      ACAround ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "space-around" styles

        else if prefix == Vendor.MS
        then style ("-ms-flex-line-pack") "distribute" styles

        else style name "space-around" styles

      _ ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) (alignContentString a) styles

        else if prefix == Vendor.MS
        then style ("-ms-flex-line-pack") (alignContentString a) styles

        else style name (alignContentString a) styles


{-| else if Set the order in which items appear in the flex container.

    import Css.Flex as Flex

    -- [ ("order", "1") ]
    Flex.order 1 []
-}
order : Int -> Styles -> Styles
order o styles =
  let prefix = Vendor.prefix
      name = "order"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) (toString o) styles

    else if prefix == Vendor.MS
    then style "-ms-flex-order" (toString o) styles

    else style name (toString o) styles


{-| else if Set the ability for a flex item to grow if necessary.
It accepts a unitless value that serves as a proportion.
It dictates what amount of the available space inside the
flex container the item should take up. Negative numbers
are invalid.

    import Css.Flex as Flex

    -- [ ("flex-grow", "1") ]
    Flex.grow 1 []
-}
grow : number -> Styles -> Styles
grow g styles =
  let prefix = Vendor.prefix
      name = "flex-grow"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) (toString g) styles

    else if prefix == Vendor.MS
    then style "-ms-flex-positive" (toString g) styles

    else style name (toString g) styles


{-| else if Set the ability for a flex item to shrink if necessary.
Negative numbers are invalid.

    import Css.Flex as Flex

    -- [ ("flex-shrink", "1") ]
    Flex.shrink 1 []
-}
shrink : number -> Styles -> Styles
shrink s styles =
  let prefix = Vendor.prefix
      name = "flex-shrink"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) (toString s) styles

    else if prefix == Vendor.MS
    then style "-ms-flex-negative" (toString s) styles

    else style name (toString s) styles


{-| else if Set the default size of an element before the remaining space is distributed.

    import Css.Flex as Flex

    -- [ ("flex-basis", "1") ]
    Flex.basis 1 []
-}
basis : number -> Styles -> Styles
basis b styles =
  let prefix = Vendor.prefix
      name = "flex-basis"
  in
    if prefix == Vendor.Webkit
    then style ("-webkit-" ++ name) (px b) styles

    else if prefix == Vendor.MS
    then style "-ms-flex-preferred-size" (px b) styles

    else style name (px b)  styles


{-| else if Set the default alignment (or the one specified by align-items)
to be overridden for individual flex items.

    import Css.Flex as Flex

    -- [ ("align-self", "flex-start") ]
    Flex.alignSelf Flex.AIStart []
-}
alignSelf : AlignItem -> Styles -> Styles
alignSelf a styles =
  let prefix = Vendor.prefix
      name = "align-self"
  in
    case a of
      AIStart ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-start" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-item-align" "start" styles

        else style name "flex-start" styles

      AIEnd ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) "flex-end" styles

        else if prefix == Vendor.MS
        then style "-ms-flex-item-align" "end" styles

        else style name "flex-end" styles

      _ ->
        if prefix == Vendor.Webkit
        then style ("-webkit-" ++ name) (alignItemString a) styles

        else if prefix == Vendor.MS
        then style "-ms-flex-item-align" (alignItemString a) styles

        else style name (alignItemString a) styles
