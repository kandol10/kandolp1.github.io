module Page.Quiz exposing (Model, Msg, init, subscriptions, toSession, update, view, nextButton)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Session exposing (Session)
import Array
import Page.CalculateIEEEFloatingPointToDecimal as Calculator exposing (..)
------------------------------------- FUNCTIONS -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--This is an array of questions
listquestions =
    Array.fromList
        [ "Convert 0 1000 001 to decimal number"
        , "Convert 1 1101 010 to decimal number"
        , "Is the floating point number positive / negative when the sign bit is 1?"
        , "Convert -4.9 into a binary number"
        , "Convert 0 1000 001 to a decimal number"
        ]


--string model = Calculator.signexponentmantissaview



stringsum model = String.fromInt(sumanwsers model) ++ " : Correct Anwsers"

howwellyoudone model = if (sumanwsers model) < 2 then (stringsum model) ++ ":/ not so bright " else if (sumanwsers model) > 2 then (stringsum model) ++ "Average" else if (sumanwsers model) == 4 then (stringsum model) ++ "You is close" else if (sumanwsers model) == 5 then (stringsum model) ++ "You is smart" else "Hello"
--This is the array of anwsers
listanwsers = Array.fromList ["1" , "2", "3", "4", "5"]
--This provides the current question you are on

--This removes 'Just'
msToString mv = Maybe.withDefault "" mv
--ToString
ts x = String.fromInt(x)

thecurrentquestion x =  div [] [text ((ts(x+1)) ++ ". " ++ msToString(Array.get x listquestions))]
-- This provides the current anwser
thecurrentanwser x = div [] [text (Debug.toString(Array.get x listanwsers))]

buttonsomething model = div (buttonStyle ("540px") ("100px")) [button [onClick ButtonSomething, style "height" "100px", style "width" "100px", style "background-color" "#3399ff", style "color" "white" ] [text "Return to Questions"]]
-- This allows the user to click 'next' to go to the next question
nextButton model = div (buttonStyle ("540px") ("100px")) [ button [ onClick Next , style "height" "100px", style "width" "100px", style "background-color" "#3399ff", style "color" "white"] [ text "Next ► " ] ]
---- This allows the user to click 'previous' to back to the previous question
previousbutton model = div (buttonStyle ("830px") ("190px")) [button [ onClick Previous, style "height" "100px", style "width" "100px", style "background-color" "#3399ff", style "color" "white" ] [ text "<◄ Previous" ]]
--This provides the total number of correct anwsers
totalcorrectanwsers model = div (questionStyle ("-50px") ("500px") ("100px")) [ text ("Total Correct Answers = " ++ String.fromInt (model.number1 + model.number + model.number3 + model.number4 + model.number5)) ]

sumanwsers model = model.number1 + model.number + model.number3 + model.number4 + model.number5

hellomodelhello model = div (questionStyle ("-50px") ("500px") ("100px")) [text ("Total Correct = " ++ String.fromInt(sumanwsers model ))]

something model = div [] [(nextButton model) , (viewQuestion model) , (viewValidation1 model) , (totalcorrectanwsers model), (previousbutton model)]

backtoquestions model = if (model.backtoquestionview == True) then (something model) else (buttomsomethinghello model)

buttomsomethinghello model = div [] [ (buttonsomething model), div [style "width" "200px", style "margin-left" "292px",style "margin-top" "250px", style "font-size" "50px"] [text (howwellyoudone model)]]

quizquestionview model = if model.question5 == "5" then (something model) else (backtoquestions model)

--This is an input for the questions
q1input model = viewInput "string" "Write anwser here..." model.question1 Question1
q2input model = viewInput "string" "Write anwser here..." model.question2 Question2
q3input model = viewInput "string" "Write anwser here..." model.question3 Question3
q4input model = viewInput "string" "Write anwser here..." model.question4 Question4
q5input model = viewInput "string" "Write anwser here..." model.question5 Question5
--textstylemantissa a y = [style "font-weight" "bold", style "font-size" "20px", style "color" y, style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "250px", style "left" "300px", style "width" "900px"]

---------------------------------- STYLES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
positiveinfinity = [style "color" "red", style "font-size" "20px", style "font-weight" "bold", style "font-family" "sansation", style "position" "fixed", style "top" "50%", style "left" "860px", style "width" "900px", style "height" "100px"]
textstyle1 x y = [style "font-weight" "bold", style "font-family" "sansation", style "position" "absolute", style "top" x, style "left" y]
buttonStyle x y = [style "position" "fixed", style "bottom""100px" , style "color" "black" , style "right" x ]
questionStyle y x a = [style "font-weight" "bold", style "position" "fixed" , style "bottom" y , style "width" "400px" , style "height" "400px" , style "height" a , style "right" x, style "font-size" "30px" ]
questionStyle1 y x a = [style "font-weight" "bold", style "position" "fixed",style "bottom" y , style "height" a , style "right" x, style "font-size" "30px" ]
buttonStyle2 = [style "height" "100px", style "width" "100px", style "background-color" "#3399ff", style "color" "white"]
------------------------------------------- view -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- This will display the next question input depending on what question input the user is on
viewValidation1 : Model -> Html Msg
viewValidation1 model =
   div (questionStyle1 ("100px") ("700px") ("200px")) [
    if model.next == 1 then q1input model
    else if model.next == 2 then q2input model
    else if model.next == 3 then q3input model
    else if model.next == 4 then q4input model
    else if model.next == 5 then q5input model
    else q1input model]



-- This will display the next question depending on what question the user is on
viewQuestion model =
    div (questionStyle ("220px") ("530px") ("200px"))
        [ if model.next == 1 then thecurrentquestion 0
          else if model.next == 2 then thecurrentquestion 1
          else if model.next == 3 then thecurrentquestion 2
          else if model.next == 4 then thecurrentquestion 3
          else if model.next == 5 then thecurrentquestion 4
          else thecurrentquestion 1 ]

type Msg
    = Todo
    | Question1 String
    | Question2 String
    | Question3 String
    | Question4 String
    | Question5 String
    | Next

    | Previous
    | ButtonSomething

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )

        Question1 question1 ->
            ( { model | question1 = question1 }, Cmd.none )

        Question2 question2 ->
            ( { model | question2 = question2 }, Cmd.none )

        Question3 question3 ->
            ( { model | question3 = question3 }, Cmd.none )

        Question4 question4 ->
            ( { model | question4 = question4 }, Cmd.none )

        Question5 question5 ->
            ( { model | question5 = question5 }, Cmd.none )

        ButtonSomething -> ( { model | backtoquestionview = if model.backtoquestionview == True then False else True}, Cmd.none)

        Next ->
            ( { model
                | next =
                    if model.next < 5 then
                        model.next + 1

                    else
                        1
                  ,number = if model.question1 == "8" then 1 else 0
                  ,number1 = if model.question2 == "2" then 1 else 0
                  ,number3 = if model.question3 == "3" then 1 else 0
                  ,number4 = if model.question4 == "4" then 1 else 0
                  ,number5 = if model.question5 == "5" then 1 else 0
              }
            , Cmd.none
            )

        Previous ->
              ( { model | next = if model.next > 1 && model.next < 6 then model.next - 1 else 1 -- Match pattern
                          ,number = if model.question1 == "2.25" then 1 else 0
                          ,number1 = if model.question2 == "−80" then 1 else 0
                          ,number3 = if model.question3 == "hello" then 1 else 0
                          ,number4 = if model.question4 == "hello" then 1 else 0
                          ,number5 = if model.question5 == "hello" then 1 else 0

              }, Cmd.none )


helloborder model = div [style "border" "3px solid", style "top" "-50px", style "left" "300px", style "width" "430px", style "height" "800px", style "position" "fixed"] [text "1"]
acalculator model = div [style "font-weight" "bold",style "text-decoration" "underline",  style "font-weight" "bold", style "left" "400px", style "top" "30px", style "position" "fixed", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ] [text "A helpa Calcultor "]
floatingPointIncrementStyle = [style "font-weight" "bold", style "left" "40px", style "top" "0px", style "position" "relative", style "border" "none", style "color" "black" , style "text-align" "center" , style "display" "inline-block", style "font-size" "30px" ]
pagestyle = [style "font-weight" "bold",style "text-decoration" "underline",  style "font-weight" "bold", style "left" "-120px", style "top" "-5px", style "position" "fixed", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ]
buttonstyle = [style "font-weight" "bold",  style "left" "40px", style "top" "0px", style "position" "relative", style "background-color" "#4CAF50", style "border" "none", style "color" "white" , style "text-align" "center" , style "display" "inline-block", style "font-size" "16px" ]
background = [style "background-color" "hsla(120, 100%, 75%, 0.3)"]
textstylemantissa1 a = [style "font-weight" "bold", style "font-size" "20px", style "color" "white", style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "200px", style "left" "300px", style "width" "900px"]



view : Model -> { title : String, content : Html Msg }
view model =     { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 pagestyle [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , helloborder model
            , acalculator model
            , quizquestionview model
          ]
        }



viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = input [ type_ t, placeholder p, value v, onInput toMsg ] []

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

toSession : Model -> Session
toSession model = model.session

type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , question1 : String
    , question2 : String
    , question3 : String
    , question4 : String
    , question5 : String
    , number : Int
    , number1 : Int
    , number3 : Int
    , number4 : Int
    , number5 : Int
    , next : Int
    , total : Int
    , previous : Int
    , backtoquestionview : Bool
    }
init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Quiz"
      , backtoquestionview = False
      , pageBody = ""
      , question1 = ""
      , question2 = ""
      , question3 = ""
      , question4 = ""
      , question5 = ""
      , number = 0
      , number1 = 0
      , number3 = 0
      , number4 = 0
      , number5 = 0
      , previous = 1
      , total = 0
      , next = 1

      }
  , Cmd.none
  )

textstylemantissa a = [style "font-weight" "bold", style "font-size" "20px", style "color" "white", style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "200px", style "left" "300px", style "width" "900px"]
