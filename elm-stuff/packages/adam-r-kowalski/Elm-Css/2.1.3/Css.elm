module Css where

{-| This module provides a few helper functions that make dealing with
css in Elm more pleasant and concise.

#Definition
@docs Styles

#String
@docs colorString

#Functions
@docs px, url, setViewport, style
-}

-- Native Imports
import Color exposing (toRgb, Color, rgba)

-- Third Party Imports
import Html exposing (Html, Attribute, div, code, text, pre, node)
import Html.Attributes exposing (style, content, name)


{-| This type alias allows us to be more expressive. -}
type alias Styles = List (String, String)


{-| Convert a color type to string form so it works with css.

    import Css exposing (colorString)

    colorString (rgba 0 0 0 1) -- "rgba(0, 0, 0, 1)"
-}
colorString : Color -> String
colorString color =
  let rgba = toRgb color
      r   = "rgba(" ++ (toString rgba.red) ++ ", "
      g   = (toString rgba.green) ++ ", "
      b   = (toString rgba.blue) ++ ", "
      a   = (toString rgba.alpha) ++ ")"
  in r ++ g ++ b ++ a


{-| Add a px post fix to the end of any integer.

    import Css exposing (px)

    px 5 -- "5px"
-}
px : number -> String
px num =
  (toString num) ++ "px "


{-| Add a url prefix to a string.

    import Css exposing (url)

    url "smiley.gif" -- "url(\"smiley.gif\")"
-}
url : String -> String
url path =
  "url(\"" ++ path ++ "\")"


{-| Set the proper width and scaling to allow for responsive design -}
setViewport : Html
setViewport =
  node "meta"
    [ name "viewport"
    , content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
    ]
    []


{-| Add a css style to a list of styles.

    import Css

    [ ("cursor", "zoom-out") ]
    Css.style "cursor" "zoom-out" []
-}
style : String -> String -> Styles -> Styles
style name value styles =
  List.append styles [ (name, value) ]
