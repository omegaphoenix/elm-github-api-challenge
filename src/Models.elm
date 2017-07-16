module Models exposing (..)

import Http exposing (Error)


---- MODEL ----


type alias Model =
  { content : String
  , users : List User
  , repos : List Repo
  , route : Route
  , client_info: Flags
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
      , client_info = flags
      }
      , Cmd.none )

type Route
  = UsersRoute
  | ReposRoute String
  | NotFoundRoute

type Msg
  = NoOp
  | Change String
  | Submit
  | SubmitUser String
  | Update (Result Http.Error (List User))
  | UpdateRepos (Result Http.Error (List Repo))
