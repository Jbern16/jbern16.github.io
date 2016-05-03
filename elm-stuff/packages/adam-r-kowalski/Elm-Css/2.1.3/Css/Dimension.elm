module Css.Dimension where

{-| Set the different dimensions an element can have.
You can set the height, maxHeight, maxWidth, minHeight, minWidth, and width.

#Height
@docs height, maxHeight, minHeight

#Width
@docs width, maxWidth, minWidth
-}

-- My Imports
import Css exposing (Styles, px)


{-| Set the height of an element.

    import Css.Dimension as Dimension

    -- [ ("height", "100px") ]
    Dimension.height 100 []
-}
height : number -> Styles -> Styles
height h styles =
  Css.style "height" (px h) styles


{-| Set the max height of an element.

    import Css.Dimension as Dimension

    -- [ ("max-height", "50px") ]
    Dimension.maxHeight 50 []
-}
maxHeight : number -> Styles -> Styles
maxHeight h styles =
  Css.style "max-height" (px h) styles


{-| Set the max width of an element.

    import Css.Dimension as Dimension

    -- [ ("max-width", "50px") ]
    Dimension.maxWidth 50 []
-}
maxWidth : number -> Styles -> Styles
maxWidth w styles =
  Css.style "max-width" (px w) styles


{-| Set the min height of an element.

    import Css.Dimension as Dimension

    -- [ ("min-height", "50px") ]
    Dimension.minHeight 50 []
-}
minHeight : number -> Styles -> Styles
minHeight h styles =
  Css.style "min-height" (px h) styles


{-| Set the min width of an element.

    import Css.Dimension as Dimension

    -- [ ("min-width", "50px") ]
    Dimension.minWidth 50 []
-}
minWidth : number -> Styles -> Styles
minWidth w styles =
  Css.style "min-width" (px w) styles


{-| Set the width of an element.

    import Css.Dimension as Dimension

    -- [ ("width", "50px") ]
    Dimension.width 50 []
-}
width : number -> Styles -> Styles
width w styles =
  Css.style "width" (px w) styles
