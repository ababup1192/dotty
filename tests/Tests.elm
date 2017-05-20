module Tests exposing (..)

-- Test modules

import PointsParser.Ast exposing (..)
import PointsParser.Parser exposing (dotsParser, parse)


--import PointsParser.Unparser exposing (toAst, unparse, unparseAst)

import Test exposing (..)
import TestExp exposing (..)


all : Test
all =
    describe "PointsParser module Test" <|
        [ parserTest
        , updatePointAstTest
        ]


parserTest : Test
parserTest =
    describe "DotsParser.Parser Test" <|
        [ "parse [(1, 2)]"
            => parse "[(1, 2)]"
            === (Ok <| rootNode [ listNode [ 0 ] [ pointNode [ 0, 0 ] [] { x = 1, y = 2 } ] ])
        , "parse [(1, 2), (3, 4)]"
            => parse "[(1, 2), (3, 4)]"
            === (Ok <| rootNode [ listNode [ 0 ] [ pointNode [ 0, 0 ] [] { x = 1, y = 2 }, pointNode [ 0, 1 ] [] { x = 3, y = 4 } ] ])
        ]


updatePointAstTest : Test
updatePointAstTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [(1, 2), (5, 6)]"
            => (parse "[(1, 2)]" |> Result.map (updatePointAst [ 0, 1 ] { x = 5, y = 6 }))
            === (Ok <| rootNode [ listNode [ 0 ] [ pointNode [ 0, 0 ] [] { x = 1, y = 2 }, pointNode [ 0, 1 ] [] { x = 5, y = 6 } ] ])
        ]



--  , "parse [(1, 2), (3, 4)]" =>
--      parse "[(1, 2), (3, 4)]" === (Ok <| Root {info = {id = 0}, ast = (NList {info = {id = 1}, asts = [NPoint {info = {id = 2}, x = 1, y = 2}, NPoint {info = {id = 3}, x = 3, y = 4}]})})
--
--  , "parse [(1, 2), (3, 4)]" =>
--      parse "[(1, 2), (3, 4)]" === (Ok <| Root <| NList [NPoint 1 2, NPoint 3 4])
--
--  , "parse []" =>
--      parse "[]" === (Ok <| Root <| NList [])
--
--  , "result equals between [(1,2),(3,4)] and [ ( 1 , 2 ) , ( 3 , 4 ) ]" =>
--      parse "[(1,2),(3,4)]" === parse "[ ( 1 , 2 ) , ( 3 , 4 ) ]"
--unparserTest : Test
--unparserTest =
--  describe "DotsParser.Unparser Test" <| [
--    "unparseAst Root <| NList []" =>
--      unparseAst (Root <| NList []) === (Ok "[]")
--
--  , "unparseAst Root <| NList [NDot 1 2]" =>
--      unparseAst (Root <| NList [NPoint 1 2]) === (Ok "[(1, 2)]")
--
--  , "unparseAst Root <| NList [NDot 1 2, NDot 3 4]" =>
--      unparseAst (Root <| NList [NPoint 1 2, NPoint 3 4]) === (Ok "[(1, 2), (3, 4)]")
--
--  , "toAst []" =>
--      toAst [] === (Root <| NList [])
--
--  , "toAst [(1, 2)]" =>
--      toAst [(1, 2)] === (Root <| NList [NPoint 1 2])
--
--  , "toAst [(1, 2), (3, 4)]" =>
--      toAst [(1, 2), (3, 4)] === (Root <| NList [NPoint 1 2, NPoint 3 4])
--
--  , "toAst [(1, 2), (3, 4)]" =>
--      toAst [(1, 2), (3, 4)] === (Root <| NList [NPoint 1 2, NPoint 3 4])
--
--  , "unparse []" =>
--      unparse [] === Ok "[]"
--
--  , "unparse [(1, 2)]" =>
--      unparse [(1, 2)] === Ok "[(1, 2)]"
--
--  , "unparse [(1, 2), (3, 4)]" =>
--      unparse [(1, 2), (3, 4)] === Ok "[(1, 2), (3, 4)]"
--  ]
