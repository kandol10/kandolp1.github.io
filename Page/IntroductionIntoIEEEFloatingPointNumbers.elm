module Page.IntroductionIntoIEEEFloatingPointNumbers exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (class)
import Session exposing (Session)
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Dict exposing (Dict)
import Json.Encode as Encode


-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , style1 : Bool
    , style2 :Bool
    , style3 : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Introduction to IEEE Floating Points"
      , pageBody = "Whatsapp appppp"
      , style1 = False
      , style2 = False
      , style3 = False

      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 pagestyle [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , hello model
            , view100 model
            , hoverexplanation model
            ]
    }


videoframe =
  iframe
  [ width 560
  , height 315
  , src "https://www.youtube.com/watch?v=oW78qOFK404"
  , property "frameborder" (Encode.string "0")
  , property "allowfullscreen" (Encode.string "true")

  ]
  []

-- UPDATE


type Msg
    = Show1
    | Hide1
    | Show2
    | Hide2
    | Show3
    | Hide3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Show1 ->
            ( { model
                | style1 = True
              }
            , Cmd.none
            )

        Hide1 ->
            ( { model
                | style1 = False
              }
            , Cmd.none
            )
        Show2 ->
            ( { model
                | style2 = True
              }
            , Cmd.none
            )

        Hide2 ->
            ( { model
                | style2 = False
              }
            , Cmd.none
            )

        Show3 ->
            ( { model
                | style3 = True
              }
            , Cmd.none
            )

        Hide3 ->
            ( { model
                | style3 = False
              }
            , Cmd.none
            )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT



toSession : Model -> Session
toSession model =
    model.session

textstyle a =  [style "background" a, style "border" "solid", style "border-color" "black", style "color" "white", style "font-weight" "bold", style "font-size" "20px"]

pagestyle = [style "font-weight" "bold",style "text-decoration" "underline",  style "font-weight" "bold", style "left" "120px", style "top" "-5px", style "position" "absolute", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ]
--linear-gradient(to bottom right, #9966ff 0%, #000066 100%)
--"linear-gradient(to bottom, #ff99ff 0%, #000066 150%)"
sp = br [] [text " "]

hello model = div
        [ style "position" "absolute"
        , style "left" "305px"
        , style "top" "300px"
        , style "height" "100px"
        , style "border" "Black"
        , style "position" "absolute"
        ,style "color" "Black" ]
        [
         p [style "font-weight" "bold", style "font-size" "25px", style "margin-left" "50px"] [ text "Example: To convert -17 into 32-bit floating point representation Sign bit = 1" ]
        , div (textstyle ("linear-gradient(to bottom, #33ccff 0%, #000066 150%"))
        [ text "1. Sign bit is the first bit of the binary representation. '1' implies negative number and '0' implies positive number"]

        , p (textstyle ("linear-gradient(to bottom right, #9966ff 0%, #000066 100%")) [text "2. Exponent is decided by the nearest smaller or equal to 2n number. For 17, 16 is the nearest 2n. Hence the exponent of 2 will be 4 since 24 = 16. 127 is the unique number for 32 bit floating point representation. It is known as bias. It is determined by 2k-1 -1 where 'k' is the number of bits in exponent field. Thus bias = 127 for 32 bit. (28-1 -1 = 128-1 = 127)"]

        , div (textstyle ("linear-gradient(to bottom, #ff99ff 0%, #000066 150%)")) [text "3. Mantissa: 17 in binary = 10001.Move the binary point so that there is only one bit from the left. Adjust the exponent of 2 so that the value does not change. This is normalizing the number. 1.0001 x 24. Now, consider the fractional part and represented as 23 bits by adding zeros."]
        ,sp
        ,div [style "font-weight" "bold"] [text "Thus the floating point representation of -17 is 1 10000011 00010000000000000000000"]
        ]


viewmantissa1 model =

  div [ style "position" "absolute"
      , style "left" "400px" , style "bottom" "-200px" , style "width" "0px" , style "height" "100%" ,style "background-color" "White" ]

      [h1
      [ style "padding" "25px"
       ,style "font-size" "40px"
       , style "right" "555px"
       , style "top" "-50px"
       , style "position" "absolute"
       ,style "font-family" "Comic Sans MS"
        ,style "background-color" "#4CAF50"
       ,style "color" "White"
      ]
      [ text "IEEE Floating Point" ]]

view100 model =

  div
        [ style "position" "absolute"
        , style "left" "400px"
        , style "bottom" "250px"
        , style "width" "0px"
        , style "height" "100%"
        ,style "background-color" "White"
        ]

        [h1
        [ style "padding" "25px"
         ,style "font-size" "40px"
         , style "right" "555px"
         , style "top" "-50px"
         , style "position" "absolute"
         ,style "font-family" "Comic Sans MS"
          ,style "background-color" "#4CAF50"
         ,style "color" "White"
        ]
        [ text "IEEE Floating Point" ]

  ,div
        [ style "position" "absolute"
        , style "left" "200px"
        , style "bottom" "80px"
        , style "width" "270px"
        , style "height" "100px"
        , style "border" "2px dashed #AAA"
        , style "text-align" "center"
        ,style "color" "Black"
        ]
        [ h1 [ style "text-align" "center"]
        [ text "0 00000 00000000" ]


  ,div
       (makeStyle1 model.style1
        ++ [onMouseEnter Show1,
            onMouseLeave Hide1,
        style "position" "absolute"
      , style "left" "0px"
      , style "top" "0px"
      , style "width" "35px"
      , style "height" "100px"
      , style "color" " #33ccff"
      , style "border-color" "#33ccff"
      , style "border" "solid"
            ]
        )
      [ h1 [ style "text-align" "center", style "border-color" "bl" ]
      [ text "" ]


  ,div
        (makeStyle1 model.style1
        ++ [ onMouseLeave Hide1,
             onMouseEnter Hide1,
        style "position" "absolute"
      , style "top" "200px"
      , style "width" "20px"
      , style "height" "50px"
      , style "right" "50"
      , style "border-width" "50px"
      , style "border-color" "#33ccff"
      ,style "color" "#33ccff"
      ]
        )
      [ h1 [style "position" "absolute", style "top" "-100px", style "text-align" "center"]
      [ text "Sign" ]
      ]
      ]

    ,div
    (makeStyle2 model.style2
     ++ [ onMouseEnter Show2,
        onMouseLeave Hide2,
           style "position" "absolute"
            , style "left" "40px"
            , style "top" "0px"
            , style "width" "75px"
            , style "height" "100px"
            , style "border" "solid"
            , style "color" " #9966ff "
            ]
          )
        [ h1 [ style "text-align" "center"]
        [ text "" ]



        ,div
        (makeStyle2 model.style2
             ++ [ onMouseLeave Hide2,
                  onMouseEnter Hide2,
                  style "position" "absolute"
                , style "top" "200px"
                , style "width" "150px"
                , style "height" "50px"
                ,style "color" "#9966ff"
                , style "border-width" "50px"
                , style "border-color" " #9966ff "
                ]
                )
            [ h1 [ style "position" "absolute", style "top" "-100px", style "text-align" "center"]
            [ text "Exponent" ]
            ]
            ]

      ,div
       (makeStyle3 model.style3
        ++[ onMouseEnter Show3,
          onMouseLeave Hide3,
             style "position" "absolute"
              , style "left" "125px"
              , style "top" "0px"
              , style "width" "141px"
              , style "height" "100px"
              , style "border" "solid"
              , style "color"  "#ff99ff"
              ]
              )
          [ h1 [ style "text-align" "center"]
          [ text "" ]



        ,div
          (makeStyle3 model.style3
           ++ [ onMouseLeave Hide3,
                onMouseEnter Hide3,
            style "position" "absolute"
                    , style "top" "200px"
                    , style "width" "150px"
                    , style "height" "50px"
                    ,style "color" "#ff99ff"
                    ]
                    )

                [ h1 [ style "position" "absolute", style "top" "-100px", style "text-align" "center"]
                [ text "Mantissa" ]
            ]
            ]
            ]
            ]


viewstyle x a= [style "border" "solid", style "font-size" "20px", style "top" "100px", style "left" x, style "position" "absolute", style "width" a, style "text-align" "center", style "width" "100%x", style "position" "absolute"]
hoverexplanation model = div (viewstyle ("332px") ("-400px"))  [text "Hover your mouse over the IEEE Floating Point to find out what bit is the sign, mantissa and exponent"]


makeStyle1 shouldShow1 =
    case shouldShow1 of
      True ->

        [
         style "opacity" "1.0"
        ]
      False ->
        [
         style "opacity" ".0"
        ]

makeStyle2 shouldShow2 =
    case shouldShow2 of
      True ->
        [
         style "opacity" "1.0"
        ]
      False ->
        [
         style "opacity" "0.0"
        ]


makeStyle3 shouldShow3 =
    case shouldShow3 of
      True ->
        [
         style "opacity" "1.0"
        ]
      False ->
        [
         style "opacity" "0.0"
        ]
