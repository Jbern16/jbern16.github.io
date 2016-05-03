module Css.Padding where

{-| Padding defines the space between the element border and the element content.

#Functions
@docs bottom, left, right, top, all
-}

-- My Imports
import Css exposing (Styles, px)


{-| Set the bottom padding of an element

    import Css.Padding as Padding

    -- [ ("padding-bottom", "20px") ]
    Padding.bottom 20 []
-}
bottom : number -> Styles -> Styles
bottom b styles =
  Css.style "padding-bottom" (px b) styles


{-| Set the left padding of an element

    import Css.Padding as Padding

    -- [ ("padding-left", "20px") ]
    Padding.left 20 []
-}
left : number -> Styles -> Styles
left l styles =
  Css.style "padding-left" (px l) styles


{-| Set the right padding of an element

    import Css.Padding as Padding

    -- [ ("padding-right", "20px") ]
    Padding.right 20 []
-}
right : number -> Styles -> Styles
right r styles =
  Css.style "padding-right" (px r) styles


{-| Set the top padding of an element

    import Css.Padding as Padding

    -- [ ("padding-top", "20px") ]
    Padding.top 20 []
-}
top : number -> Styles -> Styles
top t styles =
  Css.style "padding-top" (px t) styles


{-| Set all the padding an element has

    import Css.Padding as Padding

    {-- [ ("padding-top", "10px")
        , ("padding-right", "20px")
        , ("padding-bottom", "16px")
        , ("padding-left", "8px")]
    --}
    Padding.all 10 20 16 8 []
-}
all : number -> number -> number -> number -> Styles -> Styles
all t r b l styles =
  styles
    |> top t
    |> right r
    |> bottom b
    |> left l
