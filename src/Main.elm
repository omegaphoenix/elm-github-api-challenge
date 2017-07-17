module Main exposing (..)

import Navigation as Nav exposing (Location, programWithFlags)

import Models exposing (..)
import Routing exposing (..)
import Update exposing (..)
import View exposing (..)


---- PROGRAM ----


init : Flags -> Nav.Location -> ( Model, Cmd Msg )
init flags location =
  let
      currentRoute =
        Routing.parseLocation location
  in
      ( initialModel flags currentRoute, Cmd.none )

main : Program Flags Model Msg
main =
    Nav.programWithFlags Models.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
