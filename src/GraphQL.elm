module GraphQL
    exposing
        ( query
        , mutation
        , apply
        , maybeEncode
        )

{-| This library provides support functions used by
    [elm-graphql](https://github.com/jahewson/elm-graphql), the GraphQL code generator for Elm.

# Helper functions
@docs query, mutation, apply, maybeEncode

-}

import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Http exposing (..)


{-| Executes a GraphQL query.
-}
query : String -> String -> String -> String -> Json.Encode.Value -> Decoder a -> Request a
query method url query operation variables decoder =
    fetch method url query operation variables decoder


{-| Executes a GraphQL mutation.
-}
mutation : String -> String -> String -> Json.Encode.Value -> Decoder a -> Request a
mutation url query operation variables decoder =
    fetch "POST" url query operation variables decoder


fetch : String -> String -> String -> String -> Json.Encode.Value -> Decoder a -> Request a
fetch verb url query operation variables decoder =
    let
        request =
            buildRequestWithBody "POST" url query operation variables decoder
    in
        request


buildRequestWithBody : String -> String -> String -> String -> Json.Encode.Value -> Decoder a -> Http.Request a
buildRequestWithBody verb url query operation variables decoder =
    let
        params =
            Json.Encode.object
                [ ( "query", Json.Encode.string query )
                , ( "operationName", Json.Encode.string operation )
                , ( "variables", variables )
                ]
    in
        Http.request
            { method = verb
            , headers =
                [ (header "Accept" "application/json")
                , (header "Content-Type" "application/json")
                ]
            , url = url
            , body = Http.jsonBody <| params
            , expect = expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }


queryResult : Decoder a -> Decoder a
queryResult decoder =
    oneOf
        [ at [ "data" ] decoder
        , fail "Expected 'data' field"
          -- todo: report failure reason from server
        ]


{-| Combines two object decoders.
-}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply func value =
    map2 (<|) func value


{-| Encodes a `Maybe` as JSON, using `null` for `Nothing`.
-}
maybeEncode : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
maybeEncode e v =
    case v of
        Nothing ->
            Json.Encode.null

        Just a ->
            e a
