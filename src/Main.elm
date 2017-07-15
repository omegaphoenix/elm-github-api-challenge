module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode as JD exposing (Decoder, field, map2, nullable, string, succeed)
import Json.Decode.Pipeline as JDP exposing (decode, required, optional)
import UrlParser as  URL exposing (..)


type Route
  = UsersRoute
  | ReposRoute String
  | NotFoundRoute


---- MODEL ----


type alias Model =
  { content : String
  , users : List User
  , repos : List Repo
  , route : Route
  , client_id: String
  , client_secret: String
  }

type alias User =
  { login : String
  , avatar_url : String
  }

type alias Repo =
  { name : String
  , html_url : String
  , watchers: Int
  , language : String
  }

type alias Flags =
  { client_id : String
  , client_secret : String
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { content = ""
      , users = [
        ]
      , repos = [
        ]
      , route = UsersRoute
      , client_id = flags.client_id
      , client_secret = flags.client_secret
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
      ( { model | route = UsersRoute }, lookupUsers model.content )
    SubmitUser login ->
      ( model, lookupRepos login )
    Update (Ok res) ->
      ( { model | users = res }, Cmd.none )
    Update (Err something) ->
      let _ = Debug.log "" something
      in
      ( model, Cmd.none )
    UpdateRepos (Ok res) ->
      ( { model | repos = res, route = ReposRoute "" }, Cmd.none )
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
  JDP.decode User
      |> JDP.required "login" (JD.string)
      |> JDP.required "avatar_url" (JD.string)

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
    JDP.decode Repo
        |> JDP.required "name" (JD.string)
        |> JDP.required "html_url" (JD.string)
        |> JDP.required "watchers" (JD.int)
        |> JDP.optional "language" (JD.string) ""

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ URL.map UsersRoute top
        , URL.map ReposRoute (URL.s "users" </> URL.string)
        ]


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Search users names:"]
        , input [ placeholder "e.g. omegaphoenix", onInput Change] []
        , button [ onClick Submit ] [text "Submit"]
        , page model
        ]

page : Model -> Html Msg
page model =
    case model.route of
        UsersRoute ->
            renderUsers model.users
        ReposRoute id ->
            renderRepos model.repos
        NotFoundRoute ->
            notFoundView

notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]

renderUsers : List User -> Html Msg
renderUsers users =
    div [Html.Attributes.class "user-avatars"]
        (List.map (\user ->
          img [ onClick (SubmitUser user.login)
              , src user.avatar_url ]
              []
          ) users)

renderRepos : List Repo -> Html Msg
renderRepos repos =
    div [Html.Attributes.class "user-repos"]
        (List.map (\repo ->
          viewLink repo
          ) repos)

viewLink : Repo -> Html msg
viewLink repo =
  div []
    [ a [ href (repo.html_url) ] [ text repo.name ]
    , h1 [] [ text ("Primary language: " ++ repo.language) ]
    , h1 [] [ text ("Watchers:") ]
    ]




---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
