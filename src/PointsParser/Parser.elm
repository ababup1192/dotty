module PointsParser.Parser exposing (..)

import Parser exposing (Error, Parser, end, fail, ignore, int, oneOf, repeat, run, succeed, symbol, zeroOrMore, (|.), (|=))
import PointsParser.Ast exposing (..)


type alias ParseResult =
    Result Error Ast


parse : String -> ParseResult
parse text =
    run (dotsParser [ 0 ]) text


dotsParser : Id -> Parser Ast
dotsParser id =
    succeed (\ast -> rootNode (List.reverse id) [ ast ])
        |= expression (0 :: id)
        |. end


expression : Id -> Parser Ast
expression id =
    oneOf
        [ point <| id
        , list <| id
        ]


point : Id -> Parser Ast
point id =
    succeed (\x y -> pointNode (List.reverse id) [] { x = x, y = y })
        |. symbol "("
        |. spaces
        |= int
        |. spaces
        |. symbol ","
        |. spaces
        |= int
        |. spaces
        |. symbol ")"


list : Id -> Parser Ast
list id =
    succeed (\asts -> listNode (List.reverse id) asts)
        |. symbol "["
        |. spaces
        |= oneOf [ emptyList, oneOrMoreList id ]



-- There are ID required functions and ID NON required functions.


emptyList : Parser (List Ast)
emptyList =
    succeed []
        |. symbol "]"


oneOrMoreList : Id -> Parser (List Ast)
oneOrMoreList id =
    let
        assignIdIfNPoint index ast =
            case data ast of
                NPoint point ->
                    pointNode (List.reverse <| index :: id) [] point

                _ ->
                    ast

        assignId asts =
            List.foldl (\ast ( acc, index ) -> ( (assignIdIfNPoint index ast) :: acc, index + 1 )) ( [], 0 ) asts
    in
        succeed (\hed oth -> List.reverse << Tuple.first << assignId <| hed :: oth)
            |= point []
            |. spaces
            |= repeat zeroOrMore listHelper
            |. spaces
            |. symbol "]"


listHelper : Parser Ast
listHelper =
    succeed identity
        |. symbol ","
        |. spaces
        |= point []
        |. spaces


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


toParser : Result String a -> Parser a
toParser result =
    case result of
        Ok value ->
            succeed value

        Err message ->
            fail message
