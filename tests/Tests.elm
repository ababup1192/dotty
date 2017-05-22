module Tests exposing (..)

-- Test modules

import Test exposing (..)
import TestExp exposing (..)
import PointsParser.Ast exposing (..)
import PointsParser.Parser exposing (dotsParser, parse)
import PointsParser.Unparser exposing (unparse)


all : Test
all =
    describe "PointsParser module Test" <|
        [ parserTest
        , updatePointTest
        , unparserTest
        , insertPointTest
        , getPointsTest
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


insertPointTest : Test
insertPointTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [(1, 2), (3, 4), (5, 6)]"
            => (parse "[(1, 2), (3, 4)]" |> Result.map (insertPoint { x = 5, y = 6 }))
            === (Ok <|
                    rootNode
                        [ listNode [ 0 ]
                            [ pointNode [ 0, 0 ] [] { x = 1, y = 2 }
                            , pointNode [ 0, 1 ] [] { x = 3, y = 4 }
                            , pointNode [ 0, 2 ] [] { x = 5, y = 6 }
                            ]
                        ]
                )
        ]


updatePointTest : Test
updatePointTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [(1, 2), (5, 6)]"
            => (parse "[(1, 2), (3, 4)]" |> Result.map (updatePoint [ 0, 1 ] { x = 5, y = 6 }))
            === (Ok <| rootNode [ listNode [ 0 ] [ pointNode [ 0, 0 ] [] { x = 1, y = 2 }, pointNode [ 0, 1 ] [] { x = 5, y = 6 } ] ])
        ]


getPointsTest : Test
getPointsTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [{x = 1, y = 2}, {x = 3, y = 4), {x = 5, y = 6}]"
            => (parse "[(1, 2), (3, 4), (5, 6)]" |> Result.map getPoints)
            === (Ok <| [ { x = 1, y = 2 }, { x = 3, y = 4 }, { x = 5, y = 6 } ])
        ]


unparserTest : Test
unparserTest =
    describe "DotsParser.Unparser Test" <|
        [ "unparseAst Root <| NList []"
            => (unparse <| rootNode [ listNode [ 0 ] [] ])
            === (Ok "[]")
        , "unparseAst Root <| NList [NDot 1 2]"
            => (unparse <| rootNode [ listNode [ 0 ] [ pointNode [ 0, 0 ] [] { x = 1, y = 2 } ] ])
            === (Ok "[(1, 2)]")
        ]



--  , "result equals between [(1,2),(3,4)] and [ ( 1 , 2 ) , ( 3 , 4 ) ]" =>
--      parse "[(1,2),(3,4)]" === parse "[ ( 1 , 2 ) , ( 3 , 4 ) ]"
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
