module Main exposing (..)

import Html exposing (..)

import Models exposing (..)
import Update exposing (..)
import View exposing (..)


---- PROGRAM ----


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { content = ""
      , users = [
        ]
      , repos = [
        ]
      , route = UsersRoute
      , client_info = flags
      }
      , Cmd.none )

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
