module Page exposing (Page(..), view)
import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Route exposing (Route)

type Page
    = Other
    | IntroductionIntoIEEEFloatingPointNumbers
    | Quiz
    | CalculateIEEEFloatingPointToDecimal


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Elm SPA"
    , body =
        [ viewHeader page
        , content
        ]
    }

viewHeader : Page -> Html msg
viewHeader page =
    nav [ class "container"
        , style "position" "fixed"
        , style "top" "0"
        , style "background-color"  "grey"
        , style "color" "black"
        , style "size" "10"
        , style "padding" "20px"
        , style "height" "4000px"
        , style "width" "250px"
        , style "border" "3px solid" ]
        [ div [ class "container" ] [
             p [style "font-size" "40px", style "color" "white"] [a [if (isIntroPage page) == True then style "color" "black" else style "color" "white", Route.href Route.IntroductionIntoIEEEFloatingPointNumbers ]
                [ text "1. Introduction" ]]
             ,p [style "font-size" "40px"] [a [if (isCalculatePage page) == True then style "color" "black" else style "color" "white" , Route.href Route.CalculateIEEEFloatingPointToDecimal ]
               [ text "2. Calculator" ]]
            ,p [style "font-size" "40px"] [a [if (isQuizPage page) == True then style "color" "black" else style "color" "white", class "nav navbar-nav pull-xs-right" , Route.href Route.Quiz ]
                 [ text "3. Quiz" ]]
        ]
        ]


isIntroPage page = if page == IntroductionIntoIEEEFloatingPointNumbers then True else False
isQuizPage page = if page == Quiz then True else False
isCalculatePage page = if page == CalculateIEEEFloatingPointToDecimal then True else False


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( IntroductionIntoIEEEFloatingPointNumbers, Route.IntroductionIntoIEEEFloatingPointNumbers ) ->
            True

        ( Quiz, Route.Quiz ) ->
            True

        ( CalculateIEEEFloatingPointToDecimal, Route.CalculateIEEEFloatingPointToDecimal ) ->
            True

        _ ->
            False
