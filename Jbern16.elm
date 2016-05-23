module Jbern16 where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (fromList, get, Array )
import StartApp.Simple as StartApp
import Maybe exposing (withDefault)
import String exposing (split)

--MODEL

type alias Model =
  { headline : String
  , content : String
  , flavorText : String
  , backgroundColor : String
  , nextID : Int
  }

init : Model
init =
  { headline = "Hi, I'm Jonathan Bernesser"
  , content = "New York Based Web Developer"
  , flavorText = "Click through and learn more"
  , backgroundColor = "#3772FF"
  , nextID = 1
  }

--UPDATE

headlines : Array String
headlines =
  fromList [ " Hi, I'm Jonathan Bernesser", "Check Out My Work", "Let's Chat" ]

contents : Array String
contents =
  fromList [ "New York Based Web Developer", "https://github.com/jbern16 https://medium.com/@jBern16 https://angel.co/jonathan-bernesser", "mailto:jbern16@gmail.com https://twitter.com/jbern16 https://www.linkedin.com/in/jonathanbernesser" ]

flavorTexts : Array String
flavorTexts =
  fromList [ "Click through and learn more", "I'm currently enjoying Rails, Ruby, Elm, and Javascript", "" ]

backgroundColors : Array String
backgroundColors =
  fromList [ "#3772FF", "#1B2021", "#31AFD4" ]

changeID : Model -> Int
changeID model =
  if model.nextID == 2 then
     0
  else
    model.nextID + 1

type Action
  = NoOp
  | NextClick

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    NextClick->
      let
        headline = withDefault "" ( get (model.nextID) headlines )
        content = withDefault "" ( get (model.nextID) contents )
        flavorText = withDefault "" ( get (model.nextID) flavorTexts )
        backgroundColor = withDefault "" ( get (model.nextID ) backgroundColors )
      in
        { model | headline = headline
                , content = content
                , flavorText = flavorText
                , backgroundColor = backgroundColor
                , nextID = (changeID model) }

--VIEW


backgroundStyle : String -> Html.Attribute
backgroundStyle hex  =
  style [ ( "backgroundColor", hex)
        , ( "min-height", "100vh")
        , ( "background-position", "center")
        , ( "background-size", "cover")
        , ( "cursor", "e-resize")
        ]

textContainer : Html.Attribute
textContainer =
  style [ ( "paddingTop", "13%")
        , ( "paddingBottom", "20%")
        , ( "text-align", "center")
        , ( "color", "#E9F1F7")
        ]

border =
  style [ ( "border-style", "solid")
        , ( "border-width", "6px")
        , ( "border-color", "#E9F1F7")
        , ( "paddingTop", "30px")
        , ( "paddingBottom", "10px")
        , ( "border-radius", "3px")
        ]


iconStyle : Html.Attribute
iconStyle =
  style [ ( "padding", "10px")
        , ( "color", "white" )
        ]

sepStyle : Html.Attribute
sepStyle =
  style [ ( "font-size", "32px" ) ]

headlineStyle : Html.Attribute
headlineStyle =
  style [ ( "font-size", "48px")
        , ("font-family", "Montserrat, sans-serif")
        ]

getLink : Array String -> Int -> String
getLink content index =
  withDefault "" ( get index content )

findContent : Model -> Html
findContent model =
  if model.headline == "Let's Chat" then
    let
      links = fromList ( split " "  model.content )
      email    = getLink links 0
      twitter  = getLink links 1
      linkedIn = getLink links 2
    in
      div  [ ]
        [ a [ iconStyle, href email    ]
          [ i [ class "fa fa-envelope fa-3x" ] [ ] ]
        , a [ iconStyle, href twitter  ]
          [ i [ class "fa fa-twitter-square fa-3x"  ] [ ] ]
        , a [ iconStyle, href linkedIn ]
          [ i [ class "fa fa-linkedin-square fa-3x" ] [ ] ]
        ]

  else if model.headline == "Check Out My Work" then
    let
      links  = fromList ( split " "  model.content )
      github = getLink links 0
      medium = getLink links 1
      angel  = getLink links 2
    in
      div  [ ]
        [ a [ iconStyle, href github ]
          [ i [ class "fa fa-github-square fa-3x" ] [ ] ]
        , a [ iconStyle, href medium   ]
          [ i [ class "fa fa-medium fa-3x"   ] [ ] ]
        , a [ iconStyle, href angel ]
          [ i [ class "fa fa-angellist fa-3x" ] [ ] ]
        ]
  else
    div  [ style [ ("font-size", "24px") ] ] [ text model.content ]


findSep : String -> Html
findSep headline =
  if headline == "Let's Chat" then
    span [ sepStyle ] [ text " ° " ]
  else if headline == "Check Out My Work" then
    span [ sepStyle ] [ text " ° ° " ]
  else
    span [ sepStyle ] [ text " ° ° ° " ]

footer =
  let
    style' = style [ ( "position" , "fixed")
                   , ( "text-align", "center")
                   , ( "background-color", "#E9F1F7")
                   , ( "height", "100px" )
                   , ( "width", "100%")
                   , ( "bottom", "0%")
                   , ( "font-family", "Karla, sans-serif")
                   ]

    linkStyle = style [ ( "color", "#45503B")
                      , ( "font-size", "14px")
                      , ( "position", "relative")
                      , ( "top", "30%")
                      ]
    source = "https://github.com/Jbern16/jbern16.github.io"
  in
    div [ style' ] [
      a [ linkStyle, href source ] [ text "Made with Elm. Check it out!" ]
    ]


corner model =
  let
    next = (changeID model) - 1
    nextHex = withDefault "#31AFD4" ( get next backgroundColors )
  in
    style [ ( "width", "0" )
          , ( "height", "0" )
          , ( "border-style", "solid")
          , ( "border-width", "0 400px 400px 0")
          , ( "border-color", "transparent " ++ nextHex ++ " transparent transparent ")
          , ( "position", "fixed" )
          , ( "right", "0%")
          , ( "top", "0%")
          ]

view : Signal.Address Action -> Model -> Html
view address model =
  div [ backgroundStyle model.backgroundColor, onClick address NextClick ] [
    div [ ] [
      div [ class "small-8 small-centered columns", textContainer ] [
        div [ border ] [
          h1 [ headlineStyle ] [ text model.headline ]
          , findSep model.headline
          , div [ style [ ("font-family", "Droid Sans Mono" ) ] ] [
            findContent model
          , br [ ] [ ]
          , p  [ style [ ("font-size", "18px") ] ] [ text model.flavorText ]
          ]
        ]
      ]
    ]
  , span [ corner model ] [ ]
  , footer
  ]

main : Signal Html
main =
  StartApp.start
    { model = init,
      update = update,
      view = view
    }
