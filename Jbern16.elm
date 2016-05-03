module Jbern16 where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (fromList, get )
import Keyboard exposing (..)
import StartApp.Simple as StartApp
import Maybe exposing (withDefault)
import String exposing (repeat)

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
  fromList [ "Jonathan Bernesser", "Work", "Contact" ]

contents =
  fromList [ "Web Developer", "List Projects", "Github Blog Twitter LinkedIn Email" ]


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

view address model =
  let
    backgroundColor = withDefault "" ( get (model.nextID ) backgroundColors)
  in
    div [ backgroundStyle backgroundColor ] [
      div [ onClick address NextClick ] [
        div [ textContainer ] [
          div [ style [ ("font-size", "42px") ] ] [ text model.headline ]
        , div [ style [ ("font-size", "24px") ] ] [ text model.content ]
        , div [ style [ ("font-size", "28px") ] ] [ text " ° ° ° " ]
        ]
      ]
    ]

main =
  StartApp.start
    { model = init,
      update = update,
      view = view
    }
