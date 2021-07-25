module Main exposing (..)

import Elm.Parser
import Elm.Processing as Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.File exposing (File)
import Generators.Decoder as Decoder
import Generators.Encoder as Encoder
import Interop.JavaScript as JavaScript
import Interop.NodeJs as NodeJs
import Parser
import Regex
import Task exposing (Task)
import Utils.Imports as Imports
import Utils.Task_ as Task_


main : Program () () ()
main =
    JavaScript.cli (mainTask >> Task.mapError errorToString)



--


type Error
    = BadArguments
    | CannotParse String (List Parser.DeadEnd)
    | JavaScriptError JavaScript.Error


mainTask : { args : List String } -> Task Error String
mainTask { args } =
    let
        fileCount : List a -> String
        fileCount b =
            let
                len : Int
                len =
                    List.length b

                suffix : String
                suffix =
                    if len == 1 then
                        " module"

                    else
                        " modules"
            in
            String.fromInt len ++ suffix
    in
    case args |> List.drop 2 of
        [] ->
            Task.fail BadArguments

        a ->
            a
                |> List.map processFile
                |> Task.sequence
                |> Task.map
                    (\v ->
                        "JSON encoders/decoders generated for " ++ fileCount v ++ ".\n"
                    )


processFile : String -> Task Error String
processFile path =
    Task.map2
        (\a b ->
            Task.andThen
                (\c ->
                    processFileHelper a b c
                        |> Task.mapError JavaScriptError
                )
                (readAndParseElmFile b)
        )
        (NodeJs.scriptDirname
            |> Task.mapError JavaScriptError
        )
        (NodeJs.fileRealPath path
            |> Task.mapError JavaScriptError
        )
        |> Task.andThen identity


processFileHelper : String -> String -> RawFile -> Task JavaScript.Error String
processFileHelper binPath fullPath rawFile =
    let
        ( dirname, basename ) =
            fullPath |> split

        file : File
        file =
            rawFile |> Processing.process Processing.init |> Imports.qualifyFile
    in
    (case fullPath |> srcFolderPath of
        Just srcFolder ->
            [ NodeJs.createDirectory (srcFolder ++ "Utils/Json")
            , NodeJs.copyFile (binPath ++ "/../src/Utils/Json/Encode_.elm") (srcFolder ++ "Utils/Json/Encode_.elm")
            , NodeJs.copyFile (binPath ++ "/../src/Utils/Json/Decode_.elm") (srcFolder ++ "Utils/Json/Decode_.elm")
            ]

        Nothing ->
            []
    )
        ++ [ NodeJs.createDirectory (dirname ++ "/" ++ basename)
           , NodeJs.writeFile (dirname ++ "/" ++ basename ++ "/Encode.elm") (Encoder.fromFile file)
           , NodeJs.writeFile (dirname ++ "/" ++ basename ++ "/Decode.elm") (Decoder.fromFile file)
           ]
        |> Task.sequence
        |> Task.map (\_ -> fullPath)


readAndParseElmFile : String -> Task Error RawFile
readAndParseElmFile a =
    a
        |> NodeJs.readFile
        |> Task.mapError JavaScriptError
        |> Task.andThen
            (\v ->
                v
                    |> Elm.Parser.parse
                    |> Result.mapError (CannotParse a)
                    |> Task_.fromResult
            )



--


srcFolderPath : String -> Maybe String
srcFolderPath path =
    let
        regex : Regex.Regex
        regex =
            Regex.fromString "^.*/src/" |> Maybe.withDefault Regex.never
    in
    path
        |> Regex.find regex
        |> List.head
        |> Maybe.map .match


{-| Get dirname and basename.
-}
split : String -> ( String, String )
split a =
    case a |> String.split "/" |> List.reverse of
        b :: rest ->
            ( rest |> List.reverse |> String.join "/"
            , b |> String.split "." |> List.reverse |> List.drop 1 |> List.reverse |> String.join "."
            )

        _ ->
            ( a, "" )


errorToString : Error -> String
errorToString v =
    case v of
        BadArguments ->
            "Usage: elm-json-interop <File.elm>...\n"

        CannotParse b c ->
            "I can't parse \"" ++ b ++ "\", because: " ++ Parser.deadEndsToString c ++ ".\n"

        JavaScriptError b ->
            "elm-json-interop failed: " ++ JavaScript.errorToString b ++ "\n"
