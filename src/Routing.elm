module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Route(..))
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
  oneOf
  [ map SearchRoute top
  , map UsersRoute (s "users")
  , map ReposRoute (s "repos")
  ]


parseLocation : Location -> Route
parseLocation location =
  case (parseHash matchers location) of
    Just route ->
      route
    Nothing ->
      NotFoundRoute

usersPath : String
usersPath =
    "#users"

reposPath : String
reposPath =
    "#repos"
