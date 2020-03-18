CalculateIEEEFloatingPointToDecimal CalculateIEEEFloatingPointToDecimal.Modelmodule Page.Quiz exposing (Model, Msg, init, subscriptions, toSession, update, view, nextButton)
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
import Page.CalculateIEEEFloatingPointToDecimal exposing (..)
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

string model = model.exponent

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


----------------- THIS IS NOW THE CALCULATOR SECTION -------------------------------------------------------------------------------------------------------------------------------------------------

---------------- functions ----------------------------------------------------------------------------------------------------------------------------------S
onlicksign model a = div floatingPointIncrementStyle [
                     button [ onClick (IncrementSign) , style "font-weight" "bold", style "background-color" a, style "color" "white", style "width" "40px", style "height" "40px" ] [ text (String.fromInt (model.sign)) ]
                    , div [style "font-weight" "bold" ] [ text (String.fromInt (model.sign)) ]
                    , div [] [ text " " ]
                    ]

-- This displays the bits on the calculator i.e. whether the bit is 1 / 0 when the user clicks on the mantissa bits
onlickmantissa model x a = div floatingPointIncrementStyle [
                     button [ onClick (IncrementMantissa x), style "size-font" "40px" , style "font-weight" "bold",  style "background-color" a, style "color" "white" , style "width" "40px", style "height" "40px"] [ text ((ts x)) ]
                    , div [style "font-weight" "bold"] [ text (String.fromInt (Maybe.withDefault x (Dict.get x model.mantissa))) ]
                    , div [] [ text " " ]
                    ]

--onlickmantissa1 model x = div floatingPointIncrementStyle [ button [ onClick IncrementMantissa] [ text ("" ++ ((ts x))) ], div [] [text ""]]
-- This displays the bits on the calculator i.e. whether the bit is 1 / 0 when the user clicks on the exponent bits
onlickexponent model x a = div floatingPointIncrementStyle  [ text " "
                  , button [ onClick (IncrementExponent x),style "size" "40px", style "font-weight" "bold", style "background-color" a, style "color" "white", style "width" "40px", style "height" "40px"] [ text ((ts x)) ]
                    , div [style "font-weight" "bold"] [ text (String.fromInt (Maybe.withDefault x (Dict.get x model.exponent))) ]
                    , div [] [ text " " ]
                    ]

-- nextButton model = div (buttonStyle ("630px")) [ button [ onClick Next ] [ text "Next Question" ] ]
-- This function takes two parameters, x (position) y(what we want to multiple the position)
-- When exponent bit > 0, then it will multiply exponent by specific value i.e. for posiiton 1 it will be 2 ^ 1
-- When the exponent bit < 0, then it will multiply 0 with 2 to the power of 1
bitse model x y = div [style "float" "left"] [text ((if Maybe.withDefault x (Dict.get x model.exponent) > 0
                          then " ( 1 * 2 ^ " ++ String.fromInt(y) ++ " = " ++ String.fromInt(1 * 2 ^ y) ++ (if x == 1 then " )  = " else "  ) + ")
                          else " ( 0 * 2 ^ " ++ String.fromInt(y)  ++ (if x == 1 then " )  = " else " = 0 ) + ")))]

--This function takes two parameters, x (position) y(what we want to multiple the position)
-- When mantissa bit > 0, then it will multiple mantissa by specific value i.e. for posiiton 1 it will be 2 ^ -1
-- When the mantissa bit < 0, then it will multiply 0 with 2 to the power of -1
bitsm model x y = div [style "float" "left"] [text (if Maybe.withDefault x (Dict.get x model.mantissa) > 0
                          then " ( 1 * 2 ^ " ++ String.fromInt(y) ++ " = " ++ String.fromInt(1 * 2 ^ y) ++ (if y == -1 then ") = " else " ) + " )
                          else " ( 0 * 2 ^ " ++ String.fromInt(y)  ++ (if y == -1 then " ) = " else " = 0 ) + " ))]


acalculator model = div [style "font-weight" "bold",style "text-decoration" "underline",  style "font-weight" "bold", style "left" "400px", style "top" "30px", style "position" "fixed", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ] [text "A helpa Calcultor "]

------------------------ STYLES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------==--------------
--These are the styles
floatingPointIncrementStyle = [style "font-weight" "bold", style "left" "40px", style "top" "0px", style "position" "relative", style "border" "none", style "color" "black" , style "text-align" "center" , style "display" "inline-block", style "font-size" "30px" ]
pagestyle = [style "font-weight" "bold",style "text-decoration" "underline",  style "font-weight" "bold", style "left" "-120px", style "top" "-5px", style "position" "fixed", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ]
buttonstyle = [style "font-weight" "bold",  style "left" "40px", style "top" "0px", style "position" "relative", style "background-color" "#4CAF50", style "border" "none", style "color" "white" , style "text-align" "center" , style "display" "inline-block", style "font-size" "16px" ]
background = [style "background-color" "hsla(120, 100%, 75%, 0.3)"]
textstylemantissa1 a = [style "font-weight" "bold", style "font-size" "20px", style "color" "white", style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "200px", style "left" "300px", style "width" "900px"]

--------------------------------------------------- FUNCTIONS ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--This is making a list of the sign, mantissa and exponent in order to show a visualisation on the page
signmantissaexponentlist model = [model.sign] ++ (exponentlist model) ++ (mantissalist model)

--This changes the bits in the IEEE floating point from 0 to 1
flip bit = if bit == 1 then 0 else 1

--This is a function which allows you to get the power of 2 for a number
-- Mantissa takes two parameters, 1 for the position and one to specify what to multiply the mantissa bit by for example this may 2 ^ -1
checkexponent model x y = if Maybe.withDefault x (Dict.get x model.exponent) > 0 then 2 ^ y else 0
checkmantissa model x y = if Maybe.withDefault x (Dict.get x model.mantissa) > 0 then 2 ^ y else 0


-- This puts the values from a dictionary into a list
values : Dict k v -> List v
values dict = Dict.foldr (\key value valueList -> value :: valueList) [] dict


-- A seperate list of exponents and a seperate list of mantissas
exponentlist model = values model.exponent -- Dict.values
mantissalist model = values model.mantissa

--This is a sum of all the exponent numbers
sumexponent model = (checkexponent model 1 3) + (checkexponent model 2 2) + (checkexponent model 3 1) + (checkexponent model 4 0)

stringsumexponent model = div [style "font-weight" "bold" ] [text (s(checkexponent model 1 3) ++ " + " ++ s(checkexponent model 2 2) ++ " + " ++ s(checkexponent model 3 1) ++ " + " ++ s(checkexponent model 4 0))]
stringsummantissa model =  div [style "font-weight" "bold"] [text (s(checkmantissa model 1 -1)  ++ " + " ++  s(checkmantissa model  2 -2)  ++ " + " ++  s(checkmantissa model  3 -3)  ++ " + " ++  s(checkmantissa model  4 -4)  ++ " + " ++  s(checkmantissa model  5 -5)  ++ " + " ++  s(checkmantissa model  6 -6))]
--String.fromInt
s x = (ts x)
explist1 model = (List.length(exponentlist model)) - 1

--This is a sum of all the exponent numbers
summantissa model = (checkmantissa model 1 -1) + (checkmantissa model  2 -2) + (checkmantissa model  3 -3) + (checkmantissa model  4 -4) + (checkmantissa model  5 -5) + (checkmantissa model  6 -6)

--This is checking whether we have a positive infinity, negative infinity / nan
checkifnan model = if (List.sum (exponentlist model) == (explist1 model)) && (List.sum (mantissalist model) > 0) then True else False
checkifpositiveinfinity model =  if (model.sign == 0) && (List.sum (exponentlist model) == (explist1 model)) && (checkifnan model == False) then True else False
checkifnegativeinfinity model = if (model.sign == 1) && (List.sum (exponentlist model) == (explist1 model)) then True else False

sum list = list.sum()
--This allows the 'List String' to be converted into an html msg
renderList : List String -> Html msg
renderList lst = div [] (List.map (\l -> div [style "float" "left"] [ text l ]) lst)

--renderList only takes String as a parameter, therefore this converts each item into a String
renderingtostringfullfloating model = List.map (\a -> (String.fromInt(a))) (fplistremoval2 model)
renderingtostringexponent model = List.map (\a -> (String.fromInt(a))) (exponentlistremovl1 model)
renderingtostringmantissa model = List.map (\a -> (String.fromInt(a))) (mantissalistremovl1 model)

-- This is now putting renderingtostringfullfloating into a model, so it can be placed into the view as a html msg
floatingpointstring model = renderList(renderingtostringfullfloating model)
exponentstring model = renderList(renderingtostringexponent model)
mantissastring model =  renderList(renderingtostringmantissa model)

------------------------------ WORK ON THIS ------------------------------------
-- The following methods remove a certain position from a list
removeAt index l  =
    if index < 0 then
        l
    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
            case tail of
                Nothing ->
                    l

                Just t ->
                    List.append head t
fplistremoval1 model = removeAt 1 (signmantissaexponentlist model)
fplistremoval2 model = removeAt 5 (fplistremoval1 model)
exponentlistremovl1 model = removeAt 0 (exponentlist model)
mantissalistremovl1 model = removeAt 0 (exponentlist model)
------------------------- WORK ON THIS -------------------------------------------

-- These takes the bit model functio nd then puts them into a text, so it can seen in the view function
-- This shows the conversion in the bits model
powersofmantissa model = div [style "font-weight" "bold"] [bitsm model 1 -6, bitsm model 2 -5, bitsm model 3 -4, bitsm model 4 -3, bitsm model 5 -2, bitsm model 6 -1]
powersofexponent model = div [style "font-weight" "bold"] [div [style "float" "left"] [bitse model 4 3 ,bitse model 3 2 ,bitse model 2 1 ,bitse model 1 0 ]]

---------------------------- view --------------------------------------------------------------------------------------------------------------------------------------------------------------
helloborder model = div [style "border" "3px solid", style "top" "-50px", style "left" "300px", style "width" "430px", style "height" "800px", style "position" "fixed"] [text "1"]

viewSignIncrement model =
    div (textstyle1 ("150px") ("710px" ))
        [ onlicksign model "#3399ff"
        ]

-- This increments the corresponding bit number in the exponent
viewExponentIncrement model =
    div (textstyle1 ("150px") ("785px" ))
        [ onlickexponent model 1 "#6600ff"
        , onlickexponent model 2 "#6600ff"
        , onlickexponent model 3 "#6600ff"
        , onlickexponent model 4 "#6600ff"

        ]
-- Makes a new line
sp = br [] [text " "]
-- Allows text to be on the same line
fl = div [style "float" "left"]

expfullfp model =
    div (textstyle1 ("250px") ("750px" )) [
   fl [text "1. Floating point bits : "] , floatingpointstring model, sp, sp
   ,div [] [text ("2. The sign ' " ++ String.fromInt(model.sign) ++ " ' determines whether the floating point is positive / negative")]
   ,sp
   ,div [] [text ("3. Convert the exponent into a decimal and then work out the ex bias number")]
   , sp
    , div [] [text ("For example " ++ "eB =  ( ' "++ (String.fromInt(sumexponent model)) ++  " ' is the Exponent Decimal )  - 127 = ?")], sp
   , div [] [text " What is the exbias?" ]
   , sp
   ,div [] [text ("4. Continue the same method with the mantissa (obviously there is no exbias for this)")]
   , sp
   ,div [] [text ("5. The formular for working out the final IEEE Floating Point decimal is as follows : " )] , sp, div [] [text "(-1 ^ s) * (m) * (2^e) = "]]


--This increments the corresponding bit number in the mantissa
viewMantissaIncrement model =
    div (textstyle1 ("150px") ("950px" ))
        [ onlickmantissa model 1 "#ff66cc"
        , onlickmantissa model 2 "#ff66cc"
        , onlickmantissa model 3 "#ff66cc"
        , onlickmantissa model 4 "#ff66cc"
        , onlickmantissa model 5 "#ff66cc"
        , onlickmantissa model 6 "#ff66cc"
        ]

viewexplanation model =
    if checkifpositiveinfinity model == True then
        div (positiveinfinity) [ text "This is an error - What error is this?" ]
    else if checkifnan model == True then
        div (positiveinfinity) [ text "This is an error - What error is this?" ]
    else if checkifnegativeinfinity model == True then
        div (positiveinfinity) [ text "This is an error - What error is this?" ]
    else  expfullfp model


type Msg
    = Todo
    | Question1 String
    | Question2 String
    | Question3 String
    | Question4 String
    | Question5 String
    | Next
    | IncrementExponent Int
    | IncrementSign
    | IncrementMantissa Int
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

        IncrementExponent i ->
            ( { model
                | exponent =
                    Dict.update i (Maybe.map flip) model.exponent
              }
            , Cmd.none
            )

        IncrementMantissa i ->
            ( { model
                | mantissa =
                    Dict.update i (Maybe.map flip) model.mantissa
              }
            , Cmd.none
            )

        --9
        IncrementSign ->
            ( { model
                | sign =
                    if model.sign == 1 then
                        0

                    else
                        1
              }
            , Cmd.none
            )

view : Model -> { title : String, content : Html Msg }
view model =

    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 pagestyle [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , helloborder model
            , acalculator model
            , quizquestionview model
            , viewMantissaIncrement model
            , viewSignIncrement model
            , viewExponentIncrement model
            , viewexplanation model
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
    , sign : Int
    , exponent : Dict Int Int
    , mantissa : Dict Int Int
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
      , sign = 0
      , exponent = Dict.fromList <| List.map (\i -> ( i, 0 )) (List.range 0 4)
      , mantissa = Dict.fromList <| List.map (\i -> ( i, 0 )) (List.range 0 6)
      }
  , Cmd.none
  )

textstylemantissa a = [style "font-weight" "bold", style "font-size" "20px", style "color" "white", style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "200px", style "left" "300px", style "width" "900px"]
