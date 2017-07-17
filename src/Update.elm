module Update exposing (..)

import Http exposing (get, send)
import Json.Decode as JD exposing (Decoder, field, map2, nullable, string, succeed)
import Json.Decode.Pipeline as JDP exposing (decode, optional, required)

import Models exposing (..)
import Routing exposing (parseLocation)


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( { model | content = "" }, Cmd.none )
    Change newContent ->
      ( { model | content = newContent }, Cmd.none )
    Submit ->
      ( { model | route = UsersRoute }, lookupUsers model.content model.client_info )
    SubmitUser login ->
      ( model, lookupRepos login model.client_info )
    Update (Ok res) ->
      ( { model | users = res }, Cmd.none )
    Update (Err something) ->
      let _ = Debug.log "" something
      in
      ( model, Cmd.none )
    UpdateRepos (Ok res) ->
      ( { model | repos = res, route = ReposRoute }, Cmd.none )
    UpdateRepos (Err something) ->
      let _ = Debug.log "" something
      in
      ( model, Cmd.none )
    OnLocationChange location ->
      let
          newRoute =
            parseLocation location
      in
          ( { model | route = newRoute }, Cmd.none )

lookupUsers : String -> Flags -> Cmd Msg
lookupUsers query client_info =
  requestUsers query client_info
  |> Http.send Update

requestUsers : String -> Flags -> Http.Request (List User)
requestUsers query client_info =
  Http.get ("https://api.github.com/search/users?q=" ++ query ++ (convertClientInfo client_info)) (field "items" userListDecoder)

userListDecoder : JD.Decoder (List User)
userListDecoder =
  JD.list userDecoder
userDecoder : JD.Decoder User
userDecoder =
  JDP.decode User
      |> JDP.required "login" (JD.string)
      |> JDP.required "avatar_url" (JD.string)

lookupRepos : String -> Flags -> Cmd Msg
lookupRepos query client_info =
  requestRepos query client_info
  |> Http.send UpdateRepos

requestRepos : String -> Flags -> Http.Request (List Repo)
requestRepos query client_info =
  Http.get ("https://api.github.com/users/" ++ query ++ "/repos?" ++ (convertClientInfo client_info)) repoListDecoder

convertClientInfo : Flags -> String
convertClientInfo client_info =
  "&client_id=" ++ client_info.client_id ++ "&client_secret=" ++ client_info.client_secret

repoListDecoder : JD.Decoder (List Repo)
repoListDecoder =
  JD.list repoDecoder

repoDecoder : JD.Decoder Repo
repoDecoder =
    JDP.decode Repo
        |> JDP.required "name" (JD.string)
        |> JDP.required "html_url" (JD.string)
        |> JDP.required "watchers" (JD.int)
        |> JDP.optional "language" (JD.string) ""
