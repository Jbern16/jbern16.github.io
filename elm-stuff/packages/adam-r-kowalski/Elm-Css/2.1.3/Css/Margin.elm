module Css.Margin where

{-| Margins define the space around elements.

#Functions
@docs bottom, left, right, top, all
-}

-- My Imports
import Css exposing (Styles, px)


{-| Set the bottom margin of an element

    import Css.Margin as Margin

    -- [ ("margin-bottom", "10px")]
    Margin.bottom 10 []
-}
bottom : number -> Styles -> Styles
bottom b styles =
  Css.style "margin-bottom" (px b) styles


{-| Set the left margin of an element

    import Css.Margin as Margin

    -- [ ("margin-left", "10px")]
    Margin.left 10 []
-}
left : number -> Styles -> Styles
left l styles =
  Css.style "margin-left" (px l) styles


{-| Set the right margin of an element

    import Css.Margin as Margin

    -- [ ("margin-right", "10px")]
    Margin.right 10 []
-}
right : number -> Styles -> Styles
right r styles =
  Css.style "margin-right" (px r) styles


{-| Set the top margin of an element

    import Css.Margin as Margin

    -- [ ("margin-top", "10px")]
    Margin.top 10 []
-}
top : number -> Styles -> Styles
top t styles =
  Css.style "margin-top" (px t) styles


{-| Set all the margins of an element

    import Css.Margin as Margin

    -- [ ("margin", "10px 10px 5px 8px")]
    Margin.top 10 10 5 8 []
-}
all : number -> number -> number -> number -> Styles -> Styles
all t r b l styles =
  styles
    |> top t
    |> right r
    |> bottom b
    |> left l
