module WebDAV exposing
    ( Content, Dirent(..), Error(..)
    , ls, mkdir, mkfile, readTextFile, rm
    )

{-| WebDAV Client for Elm


# Types

@docs Content, Dirent, Error


# Manipulation

@docs ls, mkdir, mkfile, readTextFile, rm

-}

import Http exposing (Body, Resolver)
import Http.Tasks
import List.Extra
import Maybe.Extra
import Parser
import Parser.Advanced as Advanced
import Result.Extra
import Task exposing (Task)
import Url exposing (Url)
import XmlParser exposing (Node(..), Xml)



-- Types


{-| -}
type alias DeadEnd =
    Advanced.DeadEnd String Parser.Problem


{-| -}
type Error
    = HttpError Http.Error
    | ParserError (List DeadEnd)


{-| -}
type Dirent
    = File String
    | Dir String


{-| -}
type alias Content =
    { path : String
    , data : String
    }



-- Public Functions


{-| read text file
-}
readTextFile : String -> Task Error Content
readTextFile url =
    Http.Tasks.get
        { url = url
        , resolver = Http.Tasks.resolveString
        }
        |> Task.mapError HttpError
        |> Task.map (Content url)


{-| create the directory on WebDAV Storage
-}
mkdir : String -> Task Error String
mkdir url =
    mkcol { url = url }
        |> Task.mapError HttpError
        |> Task.map (always url)


{-| create the file on WebDAV Storage
-}
mkfile : String -> Body -> Task Error String
mkfile url data =
    put
        { url = url
        , body = data
        , resolver = Http.Tasks.resolveWhatever
        }
        |> Task.mapError HttpError
        |> Task.map (always url)


{-| remove file entry
-}
rm : String -> Task Error String
rm url =
    delete { url = url }
        |> Task.mapError HttpError
        |> Task.map (always url)


{-| list the file on WebDAV Storage
-}
ls : String -> Task Error (List Dirent)
ls url =
    let
        rootUrl =
            Url.fromString url
    in
    propfind { url = url }
        |> Task.mapError HttpError
        |> Task.map (stringToResult rootUrl)
        |> Task.andThen (Result.Extra.unpack Task.fail Task.succeed)



-- Private Function


{-| Create a `MKCOL` request.
-}
mkcol : { url : String } -> Task Http.Error ()
mkcol { url } =
    Http.task
        { method = "MKCOL"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.Tasks.resolveWhatever
        , timeout = Nothing
        }


{-| Create a `PROPFIND` request.
-}
propfind : { url : String } -> Task Http.Error String
propfind { url } =
    let
        data =
            String.join ""
                [ "<?xml version=\"1.0\"?>"
                , "<a:propfind xmlns:a=\"DAV:\">"
                , "<a:prop>"
                , "<a:resourcetype/>"
                , "</a:prop>"
                , "</a:propfind>"
                ]
    in
    Http.task
        { method = "PROPFIND"
        , headers = [ Http.header "Depth" "1" ]
        , url = url
        , body = Http.stringBody "text/xml" data
        , timeout = Nothing
        , resolver = Http.Tasks.resolveString
        }


{-| Create a `DELETE` request.
-}
delete : { url : String } -> Task Http.Error ()
delete { url } =
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.Tasks.resolveWhatever
        , timeout = Nothing
        }


{-| Create a `PUT` request.
-}
put : { url : String, resolver : Resolver x a, body : Body } -> Task x a
put { url, resolver, body } =
    Http.task
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , resolver = resolver
        , timeout = Nothing
        }


{-| -}
stringToResult : Maybe Url -> String -> Result Error (List Dirent)
stringToResult rootUrl data =
    XmlParser.parse data
        |> Result.map (xmlToDirent rootUrl)
        |> Result.mapError ParserError


{-| -}
xmlToDirent : Maybe Url -> Xml -> List Dirent
xmlToDirent rootUrl xml =
    childNodesByTagName "D:multistatus" [ xml.root ]
        |> Maybe.andThen List.tail
        |> Maybe.withDefault []
        |> List.map (responseToDirent rootUrl)
        |> Maybe.Extra.values


{-| -}
responseToDirent : Maybe Url -> Node -> Maybe Dirent
responseToDirent rootUrl node =
    let
        chilren =
            childNodesByTagName "D:response" [ node ]

        toDirent =
            chilren
                |> Maybe.andThen (childNodesByTagName "D:propstat")
                |> Maybe.andThen (childNodesByTagName "D:prop")
                |> Maybe.andThen (childNodesByTagName "D:resourcetype")
                |> Maybe.andThen (childNodesByTagName "D:collection")
                |> Maybe.Extra.unpack (always File) (always Dir)

        toPath href =
            Url.fromString href
                |> Maybe.map .path
                |> Maybe.withDefault href

        normalizeUrl href =
            rootUrl
                |> Maybe.map (\r -> { r | path = toPath href })
                |> Maybe.map Url.toString
    in
    chilren
        |> Maybe.andThen (childNodesByTagName "D:href")
        |> Maybe.andThen textNodeValue
        |> Maybe.andThen normalizeUrl
        |> Maybe.map toDirent



-- Node Operation


{-| -}
childNodes : Node -> Maybe (List Node)
childNodes node =
    case node of
        Element _ _ nodes ->
            Just nodes

        _ ->
            Nothing


{-| -}
nodeToString : Node -> Maybe String
nodeToString node =
    case node of
        Text s ->
            Just s

        _ ->
            Nothing


{-| -}
textNodeValue : List Node -> Maybe String
textNodeValue nodes =
    let
        condition node =
            case node of
                Text _ ->
                    True

                _ ->
                    False
    in
    List.Extra.find condition nodes
        |> Maybe.andThen nodeToString


{-| -}
childNodesByTagName : String -> List Node -> Maybe (List Node)
childNodesByTagName tagname nodes =
    let
        condition node =
            case node of
                Element a _ _ ->
                    a == tagname

                _ ->
                    False
    in
    List.Extra.find condition nodes
        |> Maybe.andThen childNodes
