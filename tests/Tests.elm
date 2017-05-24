module Tests exposing (..)

import Test exposing (..)
import ParserTest


-- Test modules


all : Test
all =
    describe "All Test" <|
        [ ParserTest.all
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
