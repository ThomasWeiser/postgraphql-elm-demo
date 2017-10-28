module Main exposing (main)

import Json.Decode exposing (Decoder)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Task
import GraphQL.Request.Builder as GB
import GraphQL.Request.Builder.Arg as GBArg
import GraphQL.Request.Builder.Variable as GBVar
import GraphQL.Client.Http as GraphQLClient


type alias Post =
    { id : Int
    , summary : Maybe String
    }


type alias Posts =
    List Post


queryPostByNodeId : GB.Document GB.Query Post { vars | nodeId : String }
queryPostByNodeId =
    let
        nodeIdVar =
            GBVar.required "nodeId" .nodeId GBVar.id

        post =
            GB.object Post
                |> GB.with (GB.field "id" [] GB.int)
                |> GB.with (GB.field "summary" [] (GB.nullable GB.string))

        queryRoot =
            GB.extract <|
                (GB.field "post"
                    [ ( "nodeId", GBArg.variable nodeIdVar ) ]
                    post
                )
    in
        GB.queryDocument queryRoot


sendQuery1 : Cmd Msg
sendQuery1 =
    queryPostByNodeId
        |> GB.request { nodeId = "WyJwb3N0cyIsMl0=" }
        |> GraphQLClient.sendQuery "http://localhost:5000/graphql"
        |> Task.attempt QueryResult


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
    , singlePost : Maybe Post
    }


type Msg
    = SearchString String
    | ApiResult (Result Http.Error Posts)
    | QueryResult (Result GraphQLClient.Error Post)


init : ( Model, Cmd Msg )
init =
    ( { posts = []
      , searchString = ""
      , singlePost = Nothing
      }
    , Cmd.batch [ getSearchPosts "", sendQuery1 ]
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

        QueryResult (Err graphQLClientError) ->
            let
                _ =
                    Debug.log "QueryResult GraphQLClient Error" graphQLClientError
            in
                ( model, Cmd.none )

        QueryResult (Ok post) ->
            ( { model | singlePost = Just post }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text (toString model.singlePost) ]
        , Html.input
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
        (Http.stringBody
            "application/graphql"
            ("query { searchPosts(search: \""
                ++ searchString
                ++ "\") { nodes { id summary } } }"
            )
        )
        (Json.Decode.at [ "data", "searchPosts", "nodes" ] decoderListOfPosts)
        |> Http.send ApiResult


decoderListOfPosts : Decoder Posts
decoderListOfPosts =
    Json.Decode.list <|
        Json.Decode.map2 Post
            (Json.Decode.field "id" Json.Decode.int)
            (Json.Decode.field "summary" <| Json.Decode.maybe Json.Decode.string)
