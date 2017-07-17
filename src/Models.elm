module Models exposing (..)

import Http exposing (Error)
import Navigation exposing (Location)


---- MODEL ----


type alias Model =
  { content : String
  , users : List User
  , repos : List Repo
  , route : Route
  , current_user : String
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

type Route
  = SearchRoute
  | UsersRoute
  | ReposRoute
  | NotFoundRoute

type Msg
  = NoOp
  | Change String
  | Submit
  | SubmitUser String
  | Update (Result Http.Error (List User))
  | UpdateRepos (Result Http.Error (List Repo))
  | OnLocationChange Location

initialModel : Flags -> Route -> Model
initialModel flags route =
  { content = ""
  , users = [
    ]
  , repos = [
    ]
  , route = route
  , current_user = ""
  , client_info = flags
  }
