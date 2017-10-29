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


type alias Cursor =
    String


type alias Page node =
    { totalCount : Int
    , pageInfo : PageInfo
    , edges : List (Edge node)
    }


type alias PageInfo =
    { startCursor : Cursor
    , endCursor : Cursor
    }


type alias Edge node =
    { cursor : Cursor
    , node : node
    }


type alias Model =
    { searchString : String
    , posts : Maybe (Page Post)
    , persons : Maybe (Page Person)
    }


type alias Person =
    { id : Int
    , firstName : String
    , posts : Maybe (Page Post)
    }


type alias Post =
    { id : Int
    , summary : Maybe String
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


valueSpecPost : GB.ValueSpec GB.NonNull GB.ObjectType Post vars
valueSpecPost =
    Post
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "summary" [] (GB.nullable GB.string))


queryAllPosts : GB.Document GB.Query (Page Post) vars
queryAllPosts =
    valueSpecPost
        |> connectionPage
        |> GB.field "allPosts" []
        |> GB.extract
        |> GB.queryDocument


querySearchPosts : GB.Document GB.Query (Page Post) { vars | search : String }
querySearchPosts =
    valueSpecPost
        |> connectionPage
        |> GB.field "searchPosts"
            [ ( "search"
              , GBArg.variable <| GBVar.required "search" .search GBVar.string
              )
            ]
        |> GB.extract
        |> GB.queryDocument


queryAllPersons : GB.Document GB.Query (Page Person) vars
queryAllPersons =
    (\id firstName -> Person id firstName Nothing)
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "firstName" [] GB.string)
        |> connectionPage
        |> GB.field "allPeople" []
        |> GB.extract
        |> GB.queryDocument


queryAllPersonsWithAllPosts : GB.Document GB.Query (Page Person) vars
queryAllPersonsWithAllPosts =
    Person
        |> GB.object
        |> GB.with (GB.field "id" [] GB.int)
        |> GB.with (GB.field "firstName" [] GB.string)
        |> GB.with
            (valueSpecPost
                |> connectionPage
                |> GB.map Just
                |> GB.field "postsByAuthorId" []
            )
        |> connectionPage
        |> GB.field "allPeople" []
        |> GB.extract
        |> GB.queryDocument


{-| Extract node objects from paginated Relay connections yielding  a plain list (without additional page- and cursor-info)
-}
connectionList :
    GB.ValueSpec GB.NonNull GB.ObjectType result vars
    -> GB.ValueSpec GB.NonNull GB.ObjectType (List result) vars
connectionList spec =
    spec
        |> GB.field "node" []
        |> GB.extract
        |> GB.list
        |> GB.field "edges" []
        |> GB.extract


{-| Extract node objects from paginated Relay connections yielding a Page type that includes all additional paging info
-}
connectionPage :
    GB.ValueSpec GB.NonNull GB.ObjectType result vars
    -> GB.ValueSpec GB.NonNull GB.ObjectType (Page result) vars
connectionPage spec =
    GB.object Page
        |> GB.with (GB.field "totalCount" [] GB.int)
        |> GB.with
            (PageInfo
                |> GB.object
                |> GB.with (GB.field "startCursor" [] GB.string)
                |> GB.with (GB.field "endCursor" [] GB.string)
                |> GB.field "pageInfo" []
            )
        |> GB.with
            (Edge
                |> GB.object
                |> GB.with (GB.field "cursor" [] GB.string)
                |> GB.with (GB.field "node" [] spec)
                |> GB.list
                |> GB.field "edges" []
            )


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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Msg
    = SearchString String
    | GraphQLClientError GraphQLClient.Error
    | QueryPostPageResult (Page Post)
    | QueryPersonPageResult (Page Person)


init : ( Model, Cmd Msg )
init =
    ( { posts = Nothing
      , persons = Nothing
      , searchString = ""
      }
    , Cmd.batch
        [ sendQuery querySearchPosts { search = "" } QueryPostPageResult
        , sendQuery queryAllPersonsWithAllPosts {} QueryPersonPageResult
          -- , sendQuery queryAllPosts {} QueryPostPageResult
          -- , sendQuery queryAllPersons {} QueryPersonPageResult
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchString str ->
            ( { model | searchString = str }
            , sendQuery querySearchPosts { search = str } QueryPostPageResult
            )

        GraphQLClientError graphQLClientError ->
            let
                _ =
                    Debug.log "... GraphQLClient Error" graphQLClientError
            in
                ( model, Cmd.none )

        QueryPostPageResult posts ->
            ( { model | posts = Just posts }
            , Cmd.none
            )

        QueryPersonPageResult persons ->
            ( { model | persons = Just persons }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.input
            [ Html.Attributes.placeholder "Search within posts"
            , Html.Events.onInput SearchString
            ]
            []
        , case model.posts of
            Nothing ->
                Html.div [] [ Html.text "[posts ...]" ]

            Just posts ->
                Html.table [] <|
                    List.map
                        (.node >> viewPost)
                        posts.edges
        , case model.persons of
            Nothing ->
                Html.div [] [ Html.text "[persons ...]" ]

            Just persons ->
                Html.table [] <|
                    List.map
                        (.node >> viewPerson)
                        persons.edges
        ]


viewPerson : Person -> Html Msg
viewPerson person =
    Html.tr []
        [ Html.td [] [ Html.text <| toString person.id ]
        , Html.td
            [ Html.Attributes.style [ ( "background-color", "linen" ) ] ]
            [ Html.text <| person.firstName ]
        , Html.td []
            [ case person.posts of
                Nothing ->
                    Html.text "[posts ...]"

                Just posts ->
                    Html.table [] <|
                        List.map
                            (.node >> viewPost)
                            posts.edges
            ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    Html.tr []
        [ Html.td [] [ Html.text <| toString post.id ]
        , Html.td [] [ Html.text <| Maybe.withDefault "[no summary]" post.summary ]
        ]
