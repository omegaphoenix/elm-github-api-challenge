module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode as JD exposing (Decoder, field, map2, string, succeed)


---- MODEL ----


type alias Model =
    { content : String
    , users : List User
    }

type alias User =
    { login : String
    , avatar_url : String
    }



init : ( Model, Cmd Msg )
init =
    ( { content = "Test"
      , users = []
      }
      , Cmd.none )



---- UPDATE ----


type Msg
  = NoOp
  | Change String
  | Submit
  | Update (Result Http.Error (List User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( { model | content = "" }, Cmd.none )
    Change newContent ->
      ( { model | content = newContent }, Cmd.none )
    Submit ->
      ( model, lookupUsers model.content )
    Update (Ok res) ->
      ( { model | users = res }, Cmd.none )
    Update (Err something) ->
      let _ = Debug.log "" something
      in
      ( model, Cmd.none )

lookupUsers : String -> Cmd Msg
lookupUsers query =
  requestUsers query
  |> Http.send Update

requestUsers : String -> Http.Request (List User)
requestUsers query =
  Http.get ("https://api.github.com/search/users?q=" ++ query) (field "items" userListDecoder)

userListDecoder : JD.Decoder (List User)
userListDecoder =
  JD.list userDecoder

userDecoder : JD.Decoder User
userDecoder =
  JD.map2 User
    (JD.field "login" JD.string)
    (JD.field "avatar_url" JD.string)

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Search users names:"]
        , input [ placeholder "e.g. omegaphoenix", onInput Change] []
        , button [ onClick Submit ] [text "Submit"]
        , renderUsers model.users
        ]


renderUsers : List User -> Html Msg
renderUsers users =
    div [Html.Attributes.class "user-avatars"]
        (List.map (\user -> img [ src user.avatar_url ] []) users)

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
