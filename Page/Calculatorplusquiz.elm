module Calculatorplusquiz exposing (Model, Msg(..), init, main, update, view)
import Browser
import Page.Quiz as Quiz
import Page.CalculateIEEEFloatingPointToDecimal as Calculator
import Html exposing (Html, button, div, h1, map, text)
import Html.Events exposing (onClick)
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
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Array


main = Browser.sandbox { init = init, update = update, view = view }


type alias Model = {calculator : Calculator.Model}


init : Model
init = {calculator = Calculator.init}

-- UPDATE

type Msg = CalculatorMsg Calculator.Msg



update : Msg -> Model -> Model
update msg model =
    case Debug.log "update msg" msg of
        Calculator calculatorMsg ->
            { model | calculator = Calculator.update calculatorMsg model.calculator }

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "" ]
        , map CalculatorMsg Calculator.view
        ]
