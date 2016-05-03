module Css.Background where

{-| Sets the background properties for the element.
You can set the color, image, position, size, repeat, and attachment.

#Definitions
@docs Repeat, Attachment

#Strings
@docs repeatString, attachmentString, pointString

#Functions
@docs color, image, position, size, repeat, attachment
-}

-- Native Imports
import Color exposing (Color, rgba)

-- My Imports
import Css exposing (Styles, colorString, px, url)


{-| Represent how to repeat the background image

Repeat -
  The background image will be repeated both vertically and horizontally.

RepeatX -
  The background image will be repeated only horizontally.
  This is really useful to repeat a gradient or some other thin
  background to save on storage space, and decrease loading time.

RepeatY -
  The background image will be repeated only vertically.

NoRepeat -
  The background image will not be repeated.
-}
type Repeat
    = Repeat
    | RepeatX
    | RepeatY
    | NoRepeat


{-| Represent whether the background image is fixed or scrolls
with the rest of the page.

Scroll -
  The background scrolls along with the element.

Fixed -
  The background is fixed with regard to the viewport.

Local -
  The background scrolls along with the element's contents
-}
type Attachment
    = Scroll
    | Fixed
    | Local


{-| Convert a repeat type to the string form so it works with css.

    import Css.Background as Background

    Background.repeatString Background.Repeat -- "repeat"
-}
repeatString : Repeat -> String
repeatString repeat =
  case repeat of
    Repeat ->
        "repeat"

    RepeatX ->
        "repeat-x"

    RepeatY ->
        "repeat-y"

    NoRepeat ->
        "no-repeat"


{-| Convert the attachment type to the string form so it works with css.

    import Css.Background as Background

    Background.attachmentString Background.Scroll -- "scroll"
-}
attachmentString : Attachment -> String
attachmentString attachment =
  case attachment of
    Scroll ->
        "scroll"

    Fixed ->
        "fixed"

    Local ->
        "local"


{-| Take two integers and append "px" so it works with css.

    import Css.Background as Background

    Background.pointString 5 10 -- "5px 10px"
-}
pointString : number -> number -> String
pointString x y =
  (px x) ++ (px y)


{-| Set the background color.

    import Css.Background as Background

    -- [ ("background-color", "rgba(255, 0, 0, 1)") ]
    Background.color (rgba 255 0 0 1) []
-}
color : Color -> Styles -> Styles
color c styles =
  Css.style "background-color" (colorString c) styles


{-| Set the background image.

    import Css.Background as Background

    -- [ ("background-image", "url(\"paper.gif\")") ]
    Background.image "paper.gif" []
-}
image : String -> Styles -> Styles
image path styles =
  Css.style "background-image" (url path) styles


{-| Set the starting position of the background image.

    import Css.Background as Background

    {-- [ ("background-image", "url(\"smiley.gif\")")
       , ("background-position", "50px 100px")
       ] --}
    Background.image "smiley.gif"
    <| Background.position 50 100 []
-}
position : number -> number -> Styles -> Styles
position x y styles =
  Css.style "background-position" (pointString x y) styles


{-| Set the size of the background image

    import Css.Background as Background

    {-- [ ("background-image", "url(\"img_flwr.gif\")")
       , ("background-size", "80px 60px")
       ] --}
    Background.image "img_flwr.gif"
    <| Background.size 80 60 []
-}
size : number -> number -> Styles -> Styles
size width height styles =
  Css.style "background-size" (pointString width height) styles


{-| Set how the background repeats

    import Css.Background as Background

    {-- [ ("background-image", "url(\"img_flwr.gif\")")
       , ("background-size", "80px 60px")
       , ("background-repeat", "no-repeat")
       ] --}
    Background.image "img_flwr.gif"
    <| Background.size 80 60
    <| Background.repeat Background.NoRepeat []
-}
repeat : Repeat -> Styles -> Styles
repeat r styles =
  Css.style "background-repeat" (repeatString r) styles


{-| Set whether a background image is fixed
or scrolls with the rest of the page.

    import Css.Background as Background

    {-- [ ("background-image", "url(\"w3css.gif\")")
       , ("background-repeat", "no-repeat")
       , ("background-attachment", "fixed")
       ] --}
    Background.image "img_flwr.gif"
    <| Background.repeat Background.NoRepeat
    <| Background.attachment Background.Fixed []
-}
attachment : Attachment -> Styles -> Styles
attachment a styles =
  Css.style "background-attachment" (attachmentString a) styles
