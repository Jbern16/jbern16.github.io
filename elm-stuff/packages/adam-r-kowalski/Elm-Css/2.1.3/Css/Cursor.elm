module Css.Cursor where

{-| Change the cursor to be displayed when pointing on an element.

#Definition
@docs Cursor

#String
@docs string

#Function
@docs cursor
-}

-- Third Party Imports
import Vendor

-- My Imports
import Css exposing (Styles, url, style)


{-| Represent the type of cursor to be displayed when pointing on an element.

Alias -
  The cursor indicates an alias of something is to be created.

AllScroll -
  The cursor indicates that something can be scrolled in any direction.

Auto -
  The browser sets a cursor.

Cell -
  The cursor indicates that a cell (or set of cells) may be selected.

ContextMenu -
  The cursor indicates that a context-menu is available.

ColResize -
  The cursor indicates that the column can be resized horizontally.

Copy -
  The cursor indicates something is to be copied.

Crosshair -
  The cursor render as a crosshair.

Default -
  The default cursor.

EResize -
  The cursor indicates that an edge of a box is to be moved right (east).

EwResize -
  Indicates a bidirectional resize cursor.

Grab -
  The cursor indicates that something can be grabbed.

Grabbing -
  The cursor indicates that something can be grabbed.

Help -
  The cursor indicates that help is available.

Move -
  The cursor indicates something is to be moved.

NResize -
  The cursor indicates that an edge of a box is to be moved up (north).

NeResize -
  The cursor indicates that an edge of a box is to be moved up
  and right (north/east).

NeswResize -
  Indicates a bidirectional resize cursor.

NsResize -
  Indicates a bidirectional resize cursor.

NwResize -
  The cursor indicates that an edge of a box is to be moved up
  and left (north/west).

NwseResize -
  Indicates a bidirectional resize cursor.

NoDrop -
  The cursor indicates that the dragged item cannot be dropped here.

None -
  No cursor is rendered for the element.

NotAllowed -
  The cursor indicates that the requested action will not be executed.

Pointer -
  The cursor is a pointer and indicates a link.

Progress -
  The cursor indicates that the program is busy (in progress).

RowResize -
  The cursor indicates that the row can be resized vertically.

SResize -
  The cursor indicates that an edge of a box is to be moved down (south).

SeResize -
  The cursor indicates that an edge of a box is to be moved down
  and right (south/east).

SwResize -
  The cursor indicates that an edge of a box is to be moved down
  and left (south/west).

Text -
  The cursor indicates text that may be selected.

Url String -
  A comma separated list of URLs to custom cursors.
  Note: Always specify a generic cursor at the end of the list,
  in case none of the URL-defined cursors can be used.

VerticalText -
  The cursor indicates vertical-text that may be selected.

WResize -
  The cursor indicates that an edge of a box is to be moved left (west).

Wait -
  The cursor indicates that the program is busy.

ZoomIn -
  The cursor indicates that something can be zoomed in.

ZoomOut -
  The cursor indicates that something can be zoomed out.
-}
type Cursor
    = Alias
    | AllScroll
    | Auto
    | Cell
    | ContextMenu
    | ColResize
    | Copy
    | Crosshair
    | Default
    | EResize
    | EwResize
    | Grab
    | Grabbing
    | Help
    | Move
    | NResize
    | NeResize
    | NeswResize
    | NsResize
    | NwResize
    | NwseResize
    | NoDrop
    | None
    | NotAllowed
    | Pointer
    | Progress
    | RowResize
    | SResize
    | SeResize
    | SwResize
    | Text
    | Url String
    | VerticalText
    | WResize
    | Wait
    | ZoomIn
    | ZoomOut


{-| Convert a cursor type to string form so it works with css.

    import Css.Cursor as Cursor

    Cursor.string Cursor.Cell -- "cell"
-}
string : Cursor -> String
string cursor =
  case cursor of
    Alias ->
        "alias"

    AllScroll ->
        "all-scroll"

    Auto ->
        "auto"

    Cell ->
        "cell"

    ContextMenu ->
        "context-menu"

    ColResize ->
        "col-resize"

    Copy ->
        "copy"

    Crosshair ->
        "crosshair"

    Default ->
        "default"

    EResize ->
        "e-resize"

    EwResize ->
        "ew-resize"

    Grab ->
        "grab"

    Grabbing ->
        "grabbign"

    Help ->
        "help"

    Move ->
        "move"

    NResize ->
        "n-resize"

    NeResize ->
        "ne-resize"

    NeswResize ->
        "nesw-resize"

    NsResize ->
        "ns-resize"

    NwResize ->
        "nw-resize"

    NwseResize ->
        "nwse-resize"

    NoDrop ->
        "no-drop"

    None ->
        "none"

    NotAllowed ->
        "not-allowed"

    Pointer ->
        "pointer"

    Progress ->
        "progress"

    RowResize ->
        "row-resize"

    SResize ->
        "s-resize"

    SeResize ->
        "se-resize"

    SwResize ->
        "sw-resize"

    Text ->
        "text"

    Url path ->
        url path

    VerticalText ->
        "vertical-text"

    WResize ->
        "w-resize"

    Wait ->
        "wait"

    ZoomIn ->
        "zoom-in"

    ZoomOut ->
        "zoom-out"


{-| Set the type of cursor to be displayed.

    import Css.Cursor as Cursor exposing (cursor)

    -- [ ("cursor", "crosshair") ]
    cursor Cursor.CrossHair []
-}
cursor : Cursor -> Styles -> Styles
cursor c styles =
  let prefix = Vendor.prefix
      name = "cursor"
  in
    case c of
      Grab ->
        if prefix == Vendor.Webkit
        then style name ("-webkit-" ++ (string c)) styles

        else style name (string c) styles

      ZoomIn ->
        if prefix == Vendor.Webkit
        then style name ("-webkit-" ++ (string c)) styles
        else style name (string c) styles

      ZoomOut ->
        if prefix == Vendor.Webkit
        then style name ("-webkit-" ++ (string c)) styles
        else style name (string c) styles

      _ ->
        style "cursor" (string c) styles
