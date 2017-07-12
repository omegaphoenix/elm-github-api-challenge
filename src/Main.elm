module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode as JD exposing (Decoder, field, map2, nullable, string, succeed)


---- MODEL ----


type alias Model =
  { content : String
  , users : List User
  , repos : List Repo
  }

type alias User =
  { login : String
  , avatar_url : String
  }

type alias Repo =
  { name : String
  , html_url : String
  , watchers_count : Int
  , language : (Maybe String)
  }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , users = [
        ]
      , repos = [
        ]
      }
      , Cmd.none )



---- UPDATE ----


type Msg
  = NoOp
  | Change String
  | Submit
  | SubmitUser String
  | Update (Result Http.Error (List User))
  | UpdateRepos (Result Http.Error (List Repo))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( { model | content = "" }, Cmd.none )
    Change newContent ->
      ( { model | content = newContent }, Cmd.none )
    Submit ->
      ( model, lookupUsers model.content )
    SubmitUser login ->
      ( model, lookupRepos login )
    Update (Ok res) ->
      ( { model | users = res }, Cmd.none )
    Update (Err something) ->
      let _ = Debug.log "" something
      in
      ( model, Cmd.none )
    UpdateRepos (Ok res) ->
      ( { model | repos = res }, Cmd.none )
    UpdateRepos (Err something) ->
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

lookupRepos : String -> Cmd Msg
lookupRepos query =
  requestRepos query
  |> Http.send UpdateRepos

requestRepos : String -> Http.Request (List Repo)
requestRepos query =
  Http.get ("https://api.github.com/users/" ++ query ++ "/repos") repoListDecoder

repoListDecoder : JD.Decoder (List Repo)
repoListDecoder =
  JD.list repoDecoder

repoDecoder : JD.Decoder Repo
repoDecoder =
  JD.map4 Repo
    (JD.field "name" JD.string)
    (JD.field "html_url" JD.string)
    (JD.field "watchers_count" JD.int)
    (JD.field "language" (JD.nullable JD.string))



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Search users names:"]
        , input [ placeholder "e.g. omegaphoenix", onInput Change] []
        , button [ onClick Submit ] [text "Submit"]
        , renderUsers model.users
        , renderRepos model.repos
        ]


renderUsers : List User -> Html Msg
renderUsers users =
    div [Html.Attributes.class "user-avatars"]
        (List.map (\user ->
          img [ onClick (SubmitUser user.login)
              , src user.avatar_url ]
          []) users)


renderRepos : List Repo -> Html Msg
renderRepos repos =
    div [Html.Attributes.class "user-repos"]
        (List.map (\repos -> div [] [ text repos.name ] ) repos)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
