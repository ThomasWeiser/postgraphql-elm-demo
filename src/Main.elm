module Main exposing (main)

import Json.Decode exposing (Decoder)
import Html exposing (Html)
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
    { posts : Posts }


type Msg
    = ApiResult (Result Http.Error Posts)


init : ( Model, Cmd Msg )
init =
    ( { posts = [] }
    , getAllPosts
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    Html.table [] <|
        List.map
            viewPost
            model.posts


viewPost : Post -> Html Msg
viewPost post =
    Html.tr []
        [ Html.td [] [ Html.text <| toString post.id ]
        , Html.td [] [ Html.text <| Maybe.withDefault "[no summary]" post.summary ]
        ]


getAllPosts : Cmd Msg
getAllPosts =
    Http.post
        "http://localhost:5000/graphql"
        (Http.stringBody "application/graphql" "query { allPosts { nodes { id summary } } }")
        decoderAllPosts
        |> Http.send ApiResult


decoderAllPosts : Decoder Posts
decoderAllPosts =
    Json.Decode.at [ "data", "allPosts", "nodes" ] <|
        Json.Decode.list <|
            Json.Decode.map2 Post
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "summary" <| Json.Decode.maybe Json.Decode.string)
