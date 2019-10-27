module Main exposing (main)

import Array
import BigInt exposing (BigInt)
import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe
import String exposing (String)
import WordList exposing (wordlist)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { text : String
    }


init : Model
init =
    { text = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | text = newContent }



-- VIEW


view : Model -> Html Msg
view model =
    let
        num =
            Maybe.withDefault (BigInt.fromInt 0) (BigInt.fromIntString model.text)
    in
    div
        []
        [ input [ placeholder "text", value model.text, onInput Change ] []
        , text (textToBip39 num)
        ]



-- LOGIC


textToBip39 : BigInt -> String
textToBip39 =
    fromDec wordlist


fromDec : Array.Array String -> BigInt -> String
fromDec signs num =
    let
        len =
            BigInt.fromInt <| Array.length signs

        index =
            Maybe.withDefault 0 <| String.toInt <| BigInt.toString <| Maybe.withDefault (BigInt.fromInt 0) <| BigInt.modBy len num

        word =
            Maybe.withDefault "" (Array.get index signs)
    in
    if BigInt.gt (BigInt.div num len) (BigInt.fromInt 0) then
        fromDec signs (BigInt.div num len) ++ " " ++ word

    else
        word
