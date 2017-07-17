module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Models exposing (..)


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
        SearchRoute ->
            renderUsers model.users
        UsersRoute ->
            renderUsers model.users
        ReposRoute ->
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
    , h1 [] [ text ("Watchers: " ++ (showWatchers repo.watchers)) ]
    ]

showWatchers : Int -> String
showWatchers numWatchers =
  showWatchersHelper numWatchers ""

showWatchersHelper : Int -> String -> String
showWatchersHelper numWatchers str =
  case numWatchers of
    0 ->
      str
    n ->
      case (n >= 100000) of
        True ->
          "💣" ++ (showWatchersHelper (n - 100000) str)
        False ->
          case (n >= 10000) of
            True ->
              "🏀" ++ (showWatchersHelper (n - 10000) str)
            False ->
              case (n >= 1000) of
                True ->
                  "👑" ++ (showWatchersHelper (n - 1000) str)
                False ->
                  case (n >= 100) of
                    True ->
                      "💯" ++ (showWatchersHelper (n - 100) str)
                    False ->
                      case (n >= 10) of
                        True ->
                          "🔟" ++ (showWatchersHelper (n - 10) str)
                        False ->
                          "🔥" ++ (showWatchersHelper (n - 1) str)
