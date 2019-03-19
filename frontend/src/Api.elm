module Api exposing (..)

import Http
import Json.Decode exposing (Value)
import Json.Encode
import Json.Decode.Pipeline exposing (required)
import Url.Builder

type alias Text = String
jsonDecText = Json.Decode.string


type alias UserInfo  =
   { id: Int
   , name: String
   , surname: String
   }

jsonDecUserInfo : Json.Decode.Decoder ( UserInfo )
jsonDecUserInfo =
   Json.Decode.succeed (\pid pname psurname -> {id = pid, name = pname, surname = psurname})
   |> required "id" (Json.Decode.int)
   |> required "name" (Json.Decode.string)
   |> required "surname" (Json.Decode.string)

jsonEncUserInfo : UserInfo -> Value
jsonEncUserInfo  val =
   Json.Encode.object
   [ ("id", Json.Encode.int val.id)
   , ("name", Json.Encode.string val.name)
   , ("surname", Json.Encode.string val.surname)
   ]



type alias CreateUserForm  =
   { name: String
   , surname: String
   }

jsonDecCreateUserForm : Json.Decode.Decoder ( CreateUserForm )
jsonDecCreateUserForm =
   Json.Decode.succeed (\pname psurname -> {name = pname, surname = psurname})
   |> required "name" (Json.Decode.string)
   |> required "surname" (Json.Decode.string)

jsonEncCreateUserForm : CreateUserForm -> Value
jsonEncCreateUserForm  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("surname", Json.Encode.string val.surname)
   ]



type alias UpdateUserForm  =
   { name: String
   , surname: String
   }

jsonDecUpdateUserForm : Json.Decode.Decoder ( UpdateUserForm )
jsonDecUpdateUserForm =
   Json.Decode.succeed (\pname psurname -> {name = pname, surname = psurname})
   |> required "name" (Json.Decode.string)
   |> required "surname" (Json.Decode.string)

jsonEncUpdateUserForm : UpdateUserForm -> Value
jsonEncUpdateUserForm  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("surname", Json.Encode.string val.surname)
   ]


getApiPing : (Result Http.Error  (Text)  -> msg) -> Cmd msg
getApiPing toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.absolute
                [ ""
                , "api"
                , "ping"
                ] []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg jsonDecText
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getApiUsersListjson : (Result Http.Error  ((List UserInfo))  -> msg) -> Cmd msg
getApiUsersListjson toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.absolute
                [ ""
                , "api"
                , "users"
                , "list.json"
                ] []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecUserInfo)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

postApiUsersCreatejson : CreateUserForm -> (Result Http.Error  (UserInfo)  -> msg) -> Cmd msg
postApiUsersCreatejson body toMsg =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.absolute
                [ ""
                , "api"
                , "users"
                , "create.json"
                ] []
        , body =
            Http.jsonBody (jsonEncCreateUserForm body)
        , expect =
            Http.expectJson toMsg jsonDecUserInfo
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

putApiUsersByUseridUpdatejson : Int -> UpdateUserForm -> (Result Http.Error  (UserInfo)  -> msg) -> Cmd msg
putApiUsersByUseridUpdatejson capture_userid body toMsg =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            Url.Builder.absolute
                [ ""
                , "api"
                , "users"
                , capture_userid |> String.fromInt
                , "update.json"
                ] []
        , body =
            Http.jsonBody (jsonEncUpdateUserForm body)
        , expect =
            Http.expectJson toMsg jsonDecUserInfo
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

deleteApiUsersByUseridDeletejson : Int -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiUsersByUseridDeletejson capture_userid toMsg =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            Url.Builder.absolute
                [ ""
                , "api"
                , "users"
                , capture_userid |> String.fromInt
                , "delete.json"
                ] []
        , body =
            Http.emptyBody
        , expect =
            Http.expectString 
                 (\x -> case x of
                 Err e -> toMsg (Err e)
                 Ok _ -> toMsg (Ok ()))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

