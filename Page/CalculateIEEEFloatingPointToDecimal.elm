module Page.CalculateIEEEFloatingPointToDecimal exposing (Model, Msg, init, subscriptions, toSession, update, view)
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

--------------------------------------------------- FUNCTIONS ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--This is making a list of the sign, mantissa and exponent in order to show a visualisation on the page

viewstyle x a= [style "border" "solid", style "font-size" "25px", style "top" "100px", style "right" x, style "position" "absolute", style "width" a, style "text-align" "center"]

signmantissaexponentlist model = [model.sign] ++ (exponentlist model) ++ (mantissalist model)

signview model = div (viewstyle ("632px") ("50px"))  [text "Sign"]
exponentview model = div (viewstyle ("460px") ("155px"))  [text "Exponent"]
mantissaview model = div (viewstyle "190px" ("241px")) [text "Mantissasa"]

signexponentmantissaview model = div [] [signview model, exponentview model, mantissaview model]


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

stringsumexponent model = div [style "font-weight" "bold" ] [text (s(checkexponent model 1 3) ++ " + " ++ s(checkexponent model 2 2) ++ " + " ++ s(checkexponent model 3 1) ++ " + " ++ s(checkexponent model 4 0) ++ " = ")]
stringsummantissa model =  div [style "font-weight" "bold"] [text (s(checkmantissa model 1 -1)  ++ " + " ++  s(checkmantissa model  2 -2)  ++ " + " ++  s(checkmantissa model  3 -3)  ++ " + " ++  s(checkmantissa model  4 -4)  ++ " + " ++  s(checkmantissa model  5 -5)  ++ " + " ++  s(checkmantissa model  6 -6) ++ " = ")]
--String.fromInt
s x = String.fromInt(x)
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

------------------------------------
--- HOW CAN I REMOVE THIS CODE TO MAKE IT SHORTER?????????????????????????
--This was needed since 0's kept appearing between each concatenation
-- This is a way in oreder to remove those zero's
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
--fplistremoval9 model = removeAt 9 (fplistremoval1 model)
----------------------------------------------------------------------------------------------
-- These takes the bit model function and then puts them into a text, so it can seen in the view function
-- This shows the conversion in the bits model
powersofmantissa model = div [style "font-weight" "bold"] [bitsm model 1 -6, bitsm model 2 -5, bitsm model 3 -4, bitsm model 4 -3, bitsm model 5 -2, bitsm model 6 -1]
powersofexponent model = div [style "font-weight" "bold"] [div [style "float" "left"] [bitse model 4 3 ,bitse model 3 2 ,bitse model 2 1 ,bitse model 1 0 ]]


-- This displays the bits on the calculator i.e. whether the bit is 1 / 0 when the user clicks on the mantissa bits
onlicksign model a = div floatingPointIncrementStyle [
                     button [ onClick (IncrementSign) , style "font-weight" "bold", style "background-color" a, style "color" "white", style "width" "40px", style "height" "40px" ] [ text (String.fromInt (model.sign)) ]
                    , div [style "font-weight" "bold" ] [ text (String.fromInt (model.sign)) ]
                    , div [] [ text " " ]
                    ]


-- This displays the bits on the calculator i.e. whether the bit is 1 / 0 when the user clicks on the mantissa bits
onlickmantissa model x a = div floatingPointIncrementStyle [
                     button [ onClick (IncrementMantissa x), style "size-font" "40px" , style "font-weight" "bold",  style "background-color" a, style "color" "white" , style "width" "40px", style "height" "40px"] [ text (String.fromInt(x)) ]
                    , div [style "font-weight" "bold"] [ text (String.fromInt (Maybe.withDefault x (Dict.get x model.mantissa))) ]
                    , div [] [ text " " ]
                    ]


--onlickmantissa1 model x = div floatingPointIncrementStyle [ button [ onClick IncrementMantissa] [ text ("" ++ (String.fromInt(x))) ], div [] [text ""]]

-- This displays the bits on the calculator i.e. whether the bit is 1 / 0 when the user clicks on the exponent bits
onlickexponent model x a = div floatingPointIncrementStyle  [ text " "
                  , button [ onClick (IncrementExponent x),style "size" "40px", style "font-weight" "bold", style "background-color" a, style "color" "white", style "width" "40px", style "height" "40px"] [ text (String.fromInt(x)) ]
                    , div [style "font-weight" "bold"] [ text (String.fromInt (Maybe.withDefault x (Dict.get x model.exponent))) ]
                    , div [] [ text " " ]
                    ]

--nextButton model = div (buttonStyle ("630px")) [ button [ onClick Next ] [ text "Next Question" ] ]

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

---------------------------------- STYLES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--These are the styles
floatingPointIncrementStyle = [style "font-weight" "bold", style "left" "40px", style "top" "0px", style "position" "relative", style "border" "none", style "color" "black" , style "text-align" "center" , style "display" "inline-block", style "font-size" "30px" ]
textstyle1 x y = [style "font-weight" "bold", style "font-family" "sansation", style "position" "absolute", style "top" x, style "left" y]
textstyle x y = [style "font-weight" "bold", style "font-family" "sansation", style "position" "absolute", style "top" x, style "left" y , style "width" "770px", style "height" "100px"]
pagestyle = [style "font-weight" "bold", style "font-weight" "bold", style "left" "150px", style "text-decoration" "underline", style "top" "-20px", style "position" "absolute", style "border" "none", style "color" "Black" , style "width" "100%", style "text-align" "center" ,style "font-size" "38px"  ]
buttonstyle = [style "font-weight" "bold",  style "left" "40px", style "top" "0px", style "position" "relative", style "background-color" "#4CAF50", style "border" "none", style "color" "white" , style "text-align" "center" , style "display" "inline-block", style "font-size" "16px" ]
background = [style "background-color" "hsla(120, 100%, 75%, 0.3)"]
textstylemantissa a y = [style "font-weight" "bold", style "font-size" "20px", style "color" y, style "border-style" "inset", style "background" a,  style "font-family" "sansation", style "position" "relative", style "top" "250px", style "left" "300px", style "width" "900px"]
positiveinfinity = [style "font-size" "50px", style "font-weight" "bold", style "font-family" "sansation", style "position" "fixed", style "top" "50%", style "left" "50%", style "width" "900px", style "height" "100px"]
----------------------- VIEW -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- This is the view, showing model

view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [h2 pagestyle [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , signexponentmantissaview model
            , viewExponentIncrement model
            , viewSignIncrement model
            , viewMantissaIncrement model
            , viewexplanation model
           ]
    }

-- This will disply either Nan, Positive Infinity, Negative Infinity - explanation
-- What it displays depends on the condiitons for example checkifnan - This checks if all bits in the floating point are 0
-- When they are all equal to 0, then this displays "NAN", else it will display an explanation on how to convert it into an IEEE Floating point number
viewexplanation model =
    if checkifpositiveinfinity model == True then
        div (positiveinfinity) [ text "Positive Infinity" ]
    else if checkifnan model == True then
        div (positiveinfinity) [ text "Nan" ]
    else if checkifnegativeinfinity model == True then
        div (positiveinfinity) [ text "Negative Infinity" ]
    else  explanationoffloatingpoint model


--This makes a space
sp = br [] [text " "]
--This puts the code on the left
fl = div [style "float" "left"]

expfullfp model =
   div (textstylemantissa ("linear-gradient(to bottom, #33ccff 0%, #000066 150%)") ("white")) [
   fl [text "1. The bits : "] , floatingpointstring model, sp, sp
   ,div [] [text "The first bit is the sign bit, the next 4 bits give the exponent and the last six bits give the mantissa."]]

explanationoffloatingmodel model =
     div (textstylemantissa ("linear-gradient(to bottom right, #9966ff 0%, #000066 100%)") ("white")) [
     fl [text "1. Working out Exponent : "], sp, sp
     ,fl [text "Lets first decode the exponent which is :"], exponentstring model, sp, sp
     , powersofexponent model, sp, sp, stringsumexponent model ,  sp, div [] [text ("Exponent Number = " ++ String.fromInt(sumexponent model))], sp
     , div [] [text " This is not yet the exponent.  If eB was the exponent, we would not be able to have numbers with a negative exponent.  To allow for negative exponents, the number eB is shifted by subtracting 127:"], sp
     , div [] [text ("eB = " ++ (String.fromInt(sumexponent model)) ++  " - 127 = " ++ (String.fromInt((sumexponent model) - 127)))], sp
     , div [] [text "Note that the extreme values eB = 0 and eB = 255 have a special meaning and would not be translated like this."]
     ]

expmantisaafp model =
     div (textstylemantissa ("linear-gradient(to bottom, #ff99ff 0%, #000066 150%)") ("white"))[
      fl [text "2. Working out Mantissa: "], sp, sp
    , fl [text " Now let us work out the mantissa :"], exponentstring model, sp, sp
     ,powersofmantissa model, sp, sp, stringsummantissa model , sp, div [] [text ("Mantissa Number = " ++ (if (summantissa model) > 0 then String.fromInt((summantissa model) + 1) else (String.fromInt(summantissa model))))], sp
     , div [] [ text "Notice that the “1+” in the calculation guarantees that m is always between 1 and 2"]
     ]


finalfp model =
     div (textstylemantissa ("white") ("black")) [
      fl [text "3. Working out full exponent number : "], sp, sp
    , div [style "float" "left"] [text "We can now decode the meaning of the whole floating-point number"], sp, sp
    , div [] [text ("The sign bit is " ++  String.fromInt(model.sign) ++ ( if (model.sign) > 0 then ( " which means the decimal is a negative number" ) else (" which means the decimal is a positive number"))) ], sp
    , div [] [text ("(-1 ^ s) * (1 + m) * (2^eB) = ")], sp
    , div [] [text (" (-1 ^ " ++ String.fromInt(model.sign) ++ ") * ( 1 + " ++  String.fromInt(summantissa model) ++ ") * (2 ^ " ++   (if (sumexponent model) > 0 then (String.fromInt((sumexponent model) - 127) ++ ")) ="  ) else String.fromInt(sumexponent model) ++ ") = "))], sp
    , div [] [text (if (summantissa model) == 0 && (sumexponent model) == 0 then "" else (String.fromInt((-1 ^ model.sign) * (1 + (summantissa model)) * (2 ^ (if (sumexponent model) > 0 then ((sumexponent model) - 127) else (sumexponent model))))))]
  ]

explanationoffloatingpoint model = div [] [expfullfp model, explanationoffloatingmodel model, expmantisaafp model, finalfp model]



-- This increments the corresponding bit number in the sign
viewSignIncrement model =
    div (textstyle1 ("145px") ("506px" ))
        [ onlicksign model "#3399ff"
        ]

-- This increments the corresponding bit number in the exponent
viewExponentIncrement model =
    div (textstyle1 ("145px") ("566px" ))
        [ onlickexponent model 1 "#6600ff"
        , onlickexponent model 2 "#6600ff"
        , onlickexponent model 3 "#6600ff"
        , onlickexponent model 4 "#6600ff"

        ]
--This increments the corresponding bit number in the mantissa
viewMantissaIncrement model =
    div (textstyle1 ("145px") ("750px" ))
        [ onlickmantissa model 1 "#ff66cc"
        , onlickmantissa model 2 "#ff66cc"
        , onlickmantissa model 3 "#ff66cc"
        , onlickmantissa model 4 "#ff66cc"
        , onlickmantissa model 5 "#ff66cc"
        , onlickmantissa model 6 "#ff66cc"
        ]

-------------------------------- UPDATE / CONTROLLER ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type Msg
    = IncrementExponent Int
    | IncrementSign
    | IncrementMantissa Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

-- MODEL
type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , sign : Int
    , exponent : Dict Int Int
    , mantissa : Dict Int Int
    }
init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "A calculator to convert IEEE Floating Point to Decimal"
      , pageBody = ""
       , sign = 0 -- The sign bit is initially equal to 0
      , exponent = Dict.fromList <| List.map (\i -> ( i, 0 )) (List.range 0 4) --This is making a dictionary for the mantissa bits
      , mantissa = Dict.fromList <| List.map (\i -> ( i, 0 )) (List.range 0 6) --This is making a dictionary for the exponent bits
      }
    , Cmd.none
    )
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
toSession : Model -> Session
toSession model = model.session
