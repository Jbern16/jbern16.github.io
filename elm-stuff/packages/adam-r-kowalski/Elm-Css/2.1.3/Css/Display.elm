module Css.Display where

{-| Set how an element is displayed, and whether it is hidden or visible.

#Definition
@docs Display

#String
@docs string

#Function
@docs display
-}

-- Third Party Imports
import Vendor


-- My Imports
import Css exposing (Styles, style)

{-| Represent the display types an element can have.

Inline -
  Displays an element as an inline element (like <span>).

Block -
  Displays an element as a block element (like <p>).

Flex -
  Displays an element as an block-level flex container.

InlineBlock -
  Displays an element as an inline-level block container.
  The inside of this block is formatted as block-level box,
  and the element itself is formatted as an inline-level box

InlineFlex -
  Displays an element as an inline-level flex container.

InlineTable -
  The element is displayed as an inline-level table.

ListItem -
  Let the element behave like a <li> element.

RunIn -
  Displays an element as either block or inline, depending on context.

Table -
  Let the element behave like a <table> element.

TableCaption -
  Let the element behave like a <caption> element.

TableColumnGroup -
  Let the element behave like a <colgroup> element.

TableHeaderGroup -
  Let the element behave like a <thead> element.

TableFooterGroup -
  Let the element behave like a <tfoot> element.

TableRowGroup -
  Let the element behave like a <tbody> element.

TableCell -
  Let the element behave like a <td> element.

TableColumn -
  Let the element behave like a <col> element.

TableRow -
  Let the element behave like a <tr> element.

None -
  The element will not be displayed at all (has no effect on layout).
-}
type Display
    = Inline
    | Block
    | Flex
    | InlineBlock
    | InlineFlex
    | InlineTable
    | ListItem
    | RunIn
    | Table
    | TableCaption
    | TableColumnGroup
    | TableHeaderGroup
    | TableFooterGroup
    | TableRowGroup
    | TableCell
    | TableColumn
    | TableRow
    | None


{-| Convert a display type to string form so it works with css.

    import Css.Display as Display

    Display.string Display.Inline -- "inline"
-}
string : Display -> String
string display =
  case display of
    Inline ->
        "inline"

    Block ->
        "block"

    Flex ->
        "flex"

    InlineBlock ->
        "inline-block"

    InlineFlex ->
        "inline-flex"

    InlineTable ->
        "inline-table"

    ListItem ->
        "list-item"

    RunIn ->
        "run-in"

    Table ->
        "table"

    TableCaption ->
        "table-caption"

    TableColumnGroup ->
        "table-column-group"

    TableHeaderGroup ->
        "table-header-group"

    TableFooterGroup ->
        "table-footer-group"

    TableRowGroup ->
        "table-row-group"

    TableCell ->
        "table-cell"

    TableColumn ->
        "table-column"

    TableRow ->
        "table-row"

    None ->
        "none"


{-| Sets the display type of an element

    import Css.Display as Display exposing (display)

    -- [ ("display", "flex") ]
    display Display.Flex []
-}
display : Display -> Styles -> Styles
display d styles =
  let prefix = Vendor.prefix
  in
    case d of
      Flex ->
        if prefix == Vendor.Webkit
        then style "display" "-webkit-flex" styles

        else if prefix == Vendor.MS
        then style "display" "-ms-flexbox" styles

        else style "display" "flex" styles

      InlineFlex ->
        if prefix == Vendor.Webkit
        then style "display" "-webkit-inline-flex" styles

        else if prefix == Vendor.MS
        then style "display" "-ms-infline-flexbox" styles

        else style "display" "flex" styles

      _ ->
        style "display" (string d) styles
