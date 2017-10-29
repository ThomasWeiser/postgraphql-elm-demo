module Main exposing (main)

import Result.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task
import GraphQL.Request.Builder as GB
import GraphQL.Request.Builder.Arg as GBArg
import GraphQL.Request.Builder.Variable as GBVar
import GraphQL.Client.Http as GraphQLClient


type alias Post =
    { id : Int
    , summary : Maybe String
    }



type alias Person =
    { id : Int
    , firstName : String
    , posts : Maybe (List Post)
    }

queryPostByNodeId : GB.Document GB.Query Post { vars | nodeId : String }
queryPostByNodeId =
    Post
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "summary" [] (GB.nullable GB.string))
        |> GB.field "post"
            [ ( "nodeId"
              , GBArg.variable <| GBVar.required "nodeId" .nodeId GBVar.id
              )
            ]
        |> GB.extract
        |> GB.queryDocument


valueSpecPosts : GB.ValueSpec GB.NonNull GB.ObjectType (List Post) vars
valueSpecPosts =
    Post
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "summary" [] (GB.nullable GB.string))
        |> connectionNodes


queryAllPosts : GB.Document GB.Query (List Post) vars
queryAllPosts =
    valueSpecPosts
        |> GB.field "allPosts" []
        |> GB.extract
        |> GB.queryDocument


querySearchPosts : GB.Document GB.Query (List Post) { vars | search : String }
querySearchPosts =
    valueSpecPosts
        |> GB.field "searchPosts"
            [ ( "search"
              , GBArg.variable <| GBVar.required "search" .search GBVar.string
              )
            ]
        |> GB.extract
        |> GB.queryDocument


queryAllPersons : GB.Document GB.Query (List Person) vars
queryAllPersons =
    (\id firstName -> Person id firstName Nothing)
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "firstName" [] GB.string)
        |> connectionNodes
        |> GB.field "allPeople" []
        |> GB.extract
        |> GB.queryDocument


queryAllPersonsWithAllPosts : GB.Document GB.Query (List Person) vars
queryAllPersonsWithAllPosts =
    Person
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "firstName" [] GB.string)
        |> GB.with (GB.field "postsByAuthorId" [] <| GB.map Just valueSpecPosts)
        |> connectionNodes
        |> GB.field "allPeople" []
        |> GB.extract
        |> GB.queryDocument



{-| A function that helps you extract node objects from paginated Relay connections.
-}
connectionNodes :
    GB.ValueSpec GB.NonNull GB.ObjectType result vars
    -> GB.ValueSpec GB.NonNull GB.ObjectType (List result) vars
connectionNodes spec =
    spec
        |> GB.field "node" []
        |> GB.extract
        |> GB.list
        |> GB.field "edges" []
        |> GB.extract


sendQuery :
    GB.Document GB.Query model vars
    -> vars
    -> (model -> Msg)
    -> Cmd Msg
sendQuery queryDocument variables successTagger =
    queryDocument
        |> GB.request variables
        |> GraphQLClient.sendQuery "http://localhost:5000/graphql"
        |> Task.attempt (Result.Extra.unpack GraphQLClientError successTagger)


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
    , posts : List Post
    , persons : List Person
    }


type Msg
    = SearchString String
    | GraphQLClientError GraphQLClient.Error
    | QueryPostsResult (List Post)
    | QueryPersonsResult (List Person)


init : ( Model, Cmd Msg )
init =
    ( { posts = []
      , persons = []
      , searchString = ""
      }
    , Cmd.batch
        [ sendQuery querySearchPosts { search = "" } QueryPostsResult
        , sendQuery queryAllPersonsWithAllPosts { } QueryPersonsResult
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchString str ->
            ( { model | searchString = str }
            , sendQuery querySearchPosts { search = str } QueryPostsResult
            )

        GraphQLClientError graphQLClientError ->
            let
                _ =
                    Debug.log "... GraphQLClient Error" graphQLClientError
            in
                ( model, Cmd.none )

        QueryPostsResult posts ->
            ( { model | posts = posts }
            , Cmd.none
            )

        QueryPersonsResult persons ->
            ( { model | persons = persons }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ {-
          Html.input
            [ Html.Attributes.placeholder "Search within posts"
            , Html.Events.onInput SearchString
            ]
            []
        , Html.table [] <|
            List.map
                viewPost
                model.posts
        , -}
          Html.table [] <|
            List.map
                viewPerson
                model.persons
        ]


viewPerson : Person -> Html Msg
viewPerson person =
    Html.tr []
        [ Html.td [] [ Html.text <| toString person.id ]
        , Html.td
            [ Html.Attributes.style [ ("background-color", "linen")]]
            [ Html.text <| person.firstName ]
        , Html.td [] 
            [ case person.posts of
                Nothing ->
                    Html.text "[posts ...]"
                Just posts ->
                    Html.table [] <|
                        List.map
                            viewPost
                            posts
            ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    Html.tr []
        [ Html.td [] [ Html.text <| toString post.id ]
        , Html.td [] [ Html.text <| Maybe.withDefault "[no summary]" post.summary ]
        ]
