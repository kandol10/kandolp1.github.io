module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Quiz as Quiz
import Page.Blank as Blank
import Page.IntroductionIntoIEEEFloatingPointNumbers as IntroductionIntoIEEEFloatingPointNumbers
import Page.CalculateIEEEFloatingPointToDecimal as CalculateIEEEFloatingPointToDecimal
import Page.NotFound as NotFound
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url exposing (Url)



type Model
    = Redirect Session
    | NotFound Session
    | IntroductionIntoIEEEFloatingPointNumbers IntroductionIntoIEEEFloatingPointNumbers.Model
    | Quiz Quiz.Model
    | CalculateIEEEFloatingPointToDecimal CalculateIEEEFloatingPointToDecimal.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        IntroductionIntoIEEEFloatingPointNumbers introductionIntoIEEEFloatingPointNumbers ->
            viewPage Page.IntroductionIntoIEEEFloatingPointNumbers GotIntroductionIntoIEEEFloatingPointNumbersMsg (IntroductionIntoIEEEFloatingPointNumbers.view introductionIntoIEEEFloatingPointNumbers)

        Quiz quiz ->
            viewPage Page.Quiz GotQuizMsg (Quiz.view quiz)

        CalculateIEEEFloatingPointToDecimal calculateIEEEFloatingPointToDecimal ->
            viewPage Page.CalculateIEEEFloatingPointToDecimal GotCalculateIEEEFloatingPointToDecimalMsg (CalculateIEEEFloatingPointToDecimal.view calculateIEEEFloatingPointToDecimal)






-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotIntroductionIntoIEEEFloatingPointNumbersMsg IntroductionIntoIEEEFloatingPointNumbers.Msg
    | GotQuizMsg Quiz.Msg
    | GotCalculateIEEEFloatingPointToDecimalMsg CalculateIEEEFloatingPointToDecimal.Msg



toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        IntroductionIntoIEEEFloatingPointNumbers introductionIntoIEEEFloatingPointNumbers ->
            IntroductionIntoIEEEFloatingPointNumbers.toSession introductionIntoIEEEFloatingPointNumbers

        Quiz quiz ->
            Quiz.toSession quiz

        CalculateIEEEFloatingPointToDecimal calculateIEEEFloatingPointToDecimal ->
            CalculateIEEEFloatingPointToDecimal.toSession calculateIEEEFloatingPointToDecimal


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.IntroductionIntoIEEEFloatingPointNumbers )

        Just Route.IntroductionIntoIEEEFloatingPointNumbers ->
            IntroductionIntoIEEEFloatingPointNumbers.init session
                |> updateWith IntroductionIntoIEEEFloatingPointNumbers GotIntroductionIntoIEEEFloatingPointNumbersMsg model

        Just Route.Quiz ->
            Quiz.init session
                |> updateWith Quiz GotQuizMsg model

        Just Route.CalculateIEEEFloatingPointToDecimal ->
            CalculateIEEEFloatingPointToDecimal.init session
                |> updateWith CalculateIEEEFloatingPointToDecimal GotCalculateIEEEFloatingPointToDecimalMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotIntroductionIntoIEEEFloatingPointNumbersMsg subMsg, IntroductionIntoIEEEFloatingPointNumbers introductionIntoIEEEFloatingPointNumbers ) ->
            IntroductionIntoIEEEFloatingPointNumbers.update subMsg introductionIntoIEEEFloatingPointNumbers
                |> updateWith IntroductionIntoIEEEFloatingPointNumbers GotIntroductionIntoIEEEFloatingPointNumbersMsg model

        ( GotQuizMsg subMsg, Quiz quiz ) ->
            Quiz.update subMsg quiz
                |> updateWith Quiz GotQuizMsg model

        ( GotCalculateIEEEFloatingPointToDecimalMsg subMsg, CalculateIEEEFloatingPointToDecimal calculateIEEEFloatingPointToDecimal ) ->
            CalculateIEEEFloatingPointToDecimal.update subMsg calculateIEEEFloatingPointToDecimal
                |> updateWith CalculateIEEEFloatingPointToDecimal GotCalculateIEEEFloatingPointToDecimalMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Sub.none

        IntroductionIntoIEEEFloatingPointNumbers introductionIntoIEEEFloatingPointNumbers ->
            Sub.map GotIntroductionIntoIEEEFloatingPointNumbersMsg (IntroductionIntoIEEEFloatingPointNumbers.subscriptions introductionIntoIEEEFloatingPointNumbers)

        Quiz quiz ->
            Sub.map GotQuizMsg (Quiz.subscriptions quiz)

        CalculateIEEEFloatingPointToDecimal calculateIEEEFloatingPointToDecimal ->
            Sub.map GotCalculateIEEEFloatingPointToDecimalMsg (CalculateIEEEFloatingPointToDecimal.subscriptions calculateIEEEFloatingPointToDecimal)



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
