module Main exposing (main)

import Json.Decode exposing (Decoder)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http


type alias Post =
    { id : Int
    , summary : Maybe String
    }


type alias Posts =
    List Post


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type alias Model =
    { searchString : String
    , posts : Posts
    }


type Msg
    = SearchString String
    | ApiResult (Result Http.Error Posts)


init : ( Model, Cmd Msg )
init =
    ( { posts = []
      , searchString = ""
      }
    , getSearchPosts ""
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchString string ->
            ( { model | searchString = string }
            , getSearchPosts string
            )

        ApiResult (Ok posts) ->
            ( { model | posts = posts }
            , Cmd.none
            )

        ApiResult (Err httpError) ->
            let
                _ =
                    Debug.log "ApiResult Http Error" httpError
            in
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.input
            [ Html.Attributes.placeholder "Search within posts"
            , Html.Events.onInput SearchString
            ]
            []
        , Html.table [] <|
            List.map
                viewPost
                model.posts
        ]


viewPost : Post -> Html Msg
viewPost post =
    Html.tr []
        [ Html.td [] [ Html.text <| toString post.id ]
        , Html.td [] [ Html.text <| Maybe.withDefault "[no summary]" post.summary ]
        ]


getSearchPosts : String -> Cmd Msg
getSearchPosts searchString =
    Http.post
        "http://localhost:5000/graphql"
        (Http.stringBody "application/graphql" ("query { searchPosts(search: \"" ++ searchString ++ "\") { nodes { id summary } } }"))
        (Json.Decode.at [ "data", "searchPosts", "nodes" ] decoderListOfPosts)
        |> Http.send ApiResult


decoderListOfPosts : Decoder Posts
decoderListOfPosts =
        Json.Decode.list <|
            Json.Decode.map2 Post
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "summary" <| Json.Decode.maybe Json.Decode.string)
