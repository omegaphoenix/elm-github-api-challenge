module Main exposing (..)

import Html exposing (..)

import Models exposing (..)
import Update exposing (..)
import View exposing (..)


---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
