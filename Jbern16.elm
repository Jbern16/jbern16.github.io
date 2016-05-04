module Jbern16 where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (fromList, get )
import StartApp.Simple as StartApp
import Maybe exposing (withDefault)
import String exposing (split)

--MODEL

type alias Model =
  { headline : String
  , content : String
  , nextID : Int
  }

init =
  { headline = "Jonathan Bernesser"
  , content = "Web Developer"
  , nextID = 1
  }

--UPDATE

headlines =
  fromList [ "Jonathan Bernesser", "My Work:", "Contact Me:" ]

contents =
  fromList [ "Web Developer", "https://github.com/jbern16", "mailto:jbern16@gmail.com, https://twitter.com/jbern16, https://medium.com/@jBern16, https://www.linkedin.com/in/jonathanbernesser" ]

changeID model =
  if model.nextID == 2 then
     0
  else
    model.nextID + 1

type Action
  = NoOp
  | NextClick

update action model =
  case action of
    NoOp ->
      model
    NextClick->
      let
        headline = withDefault "" ( get (model.nextID) headlines )
        content = withDefault "" ( get (model.nextID) contents )
      in
        { model | headline = headline
                , content = content
                , nextID = (changeID model) }

--VIEW

backgroundColors =
  fromList [ "#99D5C9", "#007EA7", "#5E8C61" ]

backgroundStyle backgroundColor =
  style [ ( "backgroundColor", backgroundColor)
        , ( "height", "100vh")
        ]

textContainer =
  style [ ( "paddingTop", "20%")
        , ( "paddingBottom", "20%")
        , ( "text-align", "center")
        , ( "font-family", "Montserrat, sans-serif")
        ]

iconStyle =
  style [ ( "padding", "10px")
        , ( "color", "black" )
        ]

getLink content index =
  withDefault "" ( get index content )

findContent model =
  if model.headline == "Contact Me:" then
    let
      links = fromList ( split ","  model.content )
      email    = getLink links 0
      twitter  = getLink links 1
      medium   = getLink links 2
      linkedIn = getLink links 3
    in
      div [ ]
        [ a [ iconStyle, href email    ]
          [ i [ class "fa fa-envelope fa-3x" ] [ ] ]
        , a [ iconStyle, href twitter  ]
          [ i [ class "fa fa-twitter fa-3x"  ] [ ] ]
        , a [ iconStyle, href medium   ]
          [ i [ class "fa fa-medium fa-3x"   ] [ ] ]
        , a [ iconStyle, href linkedIn ]
          [ i [ class "fa fa-linkedin fa-3x" ] [ ] ]
        ]

  else if model.headline == "My Work:" then
    let
      links = fromList ( split ","  model.content )
      github = getLink links 0
    in
      div [ ]
        [ a [ iconStyle, href github ]
          [ i [ class "fa fa-github fa-3x" ] [ ] ]
        ]
  else
    div [ style [ ("font-size", "24px") ] ] [ text model.content ]


view address model =
  let
    backgroundColor = withDefault "" ( get (model.nextID ) backgroundColors)
  in
    div [ backgroundStyle backgroundColor ] [
      div [ onClick address NextClick ] [
        div [ textContainer ] [
          div [ style [ ("font-size", "42px") ] ] [ text model.headline ]
        , div [ style [ ("font-size", "28px") ] ] [ text " ° ° ° " ]
        , findContent model
        ]
      ]
    ]

main =
  StartApp.start
    { model = init,
      update = update,
      view = view
    }
