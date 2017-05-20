module PointsParser.Parser exposing (..)

import Parser exposing (Error, Parser, end, fail, ignore, int, oneOf, repeat, run, succeed, symbol, zeroOrMore, (|.), (|=))
import PointsParser.Ast exposing (Ast(NList, NPoint, Root), Id)


type alias ParseResult =
    Result Error Ast


parse : String -> ParseResult
parse text =
    run (dotsParser 0) text


dotsParser : Id -> Parser Ast
dotsParser id =
    succeed (\ast -> Root {info = {id = id}, ast = ast})
        |= expression (id + 1)
        |. end


expression : Id -> Parser Ast
expression id =
    oneOf
        [ point id
        , list id
        ]


point : Id -> Parser Ast
point id =
    succeed (\x y -> NPoint {info = {id = id}, x = x, y = y})
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
    succeed (\asts -> NList {info = {id = id}, asts = asts})
        |. symbol "["
        |. spaces
        |= oneOf [ emptyList, oneOrMoreList <| id + 1 ]

-- There are ID required functions and ID NON required functions.
emptyList : Parser (List Ast)
emptyList =
    succeed []
        |. symbol "]"


oneOrMoreList : Id -> Parser (List Ast)
oneOrMoreList id =
    let
        assignIdIfNPoint id ast =
            case ast of
                NPoint point ->
                    NPoint {point| info = {id = id}}

                _ -> ast

        assignId asts =
            List.foldl (\ast (acc, id_) -> ((assignIdIfNPoint id_ ast) :: acc, id_ + 1)) ([], id) asts
    in
    succeed (\hed oth -> List.reverse << Tuple.first << assignId <| hed :: oth)
        |= point -1
        |. spaces
        |= repeat zeroOrMore listHelper
        |. spaces
        |. symbol "]"


listHelper : Parser Ast
listHelper =
    succeed identity
        |. symbol ","
        |. spaces
        |= point -1
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
