module PointsParser.Parser exposing (..)

import Parser exposing (Error, Parser, end, ignore, int, oneOf, repeat, run, succeed, symbol, zeroOrMore, (|.), (|=))
import PointsParser.Ast exposing (Ast(NPoint, NList, Root))


type alias ParseResult =
    Result Error Ast


parse : String -> ParseResult
parse text =
    run dotsParser text


dotsParser : Parser Ast
dotsParser =
    succeed Root
        |= expression
        |. end


expression : Parser Ast
expression =
    oneOf
        [ point
        , list
        ]


point : Parser Ast
point =
    succeed NPoint
        |. symbol "("
        |. spaces
        |= int
        |. spaces
        |. symbol ","
        |. spaces
        |= int
        |. spaces
        |. symbol ")"


list : Parser Ast
list =
    succeed NList
        |. symbol "["
        |. spaces
        |= oneOf [ emptyList, oneOrMoreList ]


emptyList : Parser (List Ast)
emptyList =
    succeed []
        |. symbol "]"


oneOrMoreList : Parser (List Ast)
oneOrMoreList =
    succeed (::)
        |= point
        |. spaces
        |= repeat zeroOrMore listHelper
        |. spaces
        |. symbol "]"


listHelper : Parser Ast
listHelper =
    succeed identity
        |. symbol ","
        |. spaces
        |= point
        |. spaces


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
