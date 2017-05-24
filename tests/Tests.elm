module Tests exposing (..)

-- Test modules

import Test exposing (..)
import TestExp exposing (..)
import DotsParser.Ast exposing (..)
import DotsParser.Parser exposing (dotsParser, parse)
import DotsParser.Unparser exposing (unparse)


all : Test
all =
    describe "PointsParser module Test" <|
        [ parserTest
        , updatePositionTest
        , unparserTest
        , getPositionTest
        , insertPositionTest
        , ast2PositionsTest
        ]


parserTest : Test
parserTest =
    describe "DotsParser.Parser Test" <|
        [ "parse [(1, 2)]"
            => parse "[(1, 2)]"
            === (Ok <| rootNode [ listNode [ 0 ] [ positionNode [ 0, 0 ] [] { x = 1, y = 2 } ] ])
        , "parse [(1, 2), (3, 4)]"
            => parse "[(1, 2), (3, 4)]"
            === (Ok <| rootNode [ listNode [ 0 ] [ positionNode [ 0, 0 ] [] { x = 1, y = 2 }, positionNode [ 0, 1 ] [] { x = 3, y = 4 } ] ])
        ]


getPositionTest : Test
getPositionTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> Just { x = 3, y = 4}]"
            => (parse "[(1, 2), (3, 4)]" |> Result.map (getPosition [ 0, 1 ]))
            === (Ok <| Just { x = 3, y = 4 })
        ]


insertPositionTest : Test
insertPositionTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [(1, 2), (3, 4), (5, 6)]"
            => (parse "[(1, 2), (3, 4)]" |> Result.map (insertPosition { x = 5, y = 6 }))
            === (Ok <|
                    rootNode
                        [ listNode [ 0 ]
                            [ positionNode [ 0, 0 ] [] { x = 1, y = 2 }
                            , positionNode [ 0, 1 ] [] { x = 3, y = 4 }
                            , positionNode [ 0, 2 ] [] { x = 5, y = 6 }
                            ]
                        ]
                )
        ]


updatePositionTest : Test
updatePositionTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [(1, 2), (5, 6)]"
            => (parse "[(1, 2), (3, 4)]" |> Result.map (updatePosition [ 0, 1 ] { x = 5, y = 6 }))
            === (Ok <| rootNode [ listNode [ 0 ] [ positionNode [ 0, 0 ] [] { x = 1, y = 2 }, positionNode [ 0, 1 ] [] { x = 5, y = 6 } ] ])
        ]


ast2PositionsTest : Test
ast2PositionsTest =
    describe "DotsParser.Ast Test" <|
        [ "[(1, 2), (3, 4)] -> [{x = 1, y = 2}, {x = 3, y = 4), {x = 5, y = 6}]"
            => (parse "[(1, 2), (3, 4), (5, 6)]" |> Result.map ast2Positions)
            === (Ok <|
                    [ { id = [ 0, 0 ], position = { x = 1, y = 2 } }
                    , { id = [ 0, 1 ], position = { x = 3, y = 4 } }
                    , { id = [ 0, 2 ], position = { x = 5, y = 6 } }
                    ]
                )
        ]


unparserTest : Test
unparserTest =
    describe "DotsParser.Unparser Test" <|
        [ "unparseAst Root <| NList []"
            => unparse (rootNode [ listNode [ 0 ] [] ]) Nothing
            === (Ok "[]")
        , "unparseAst Root <| NList [NPoint 1 2]"
            => unparse (rootNode [ listNode [ 0 ] [ positionNode [ 0, 0 ] [] { x = 1, y = 2 } ] ]) Nothing
            === (Ok "[(1, 2)]")
        , "unparseAst Root <| NList [NPoint 1 2, 3 4]"
            => unparse (rootNode [ listNode [ 0 ] [ positionNode [ 0, 0 ] [] { x = 1, y = 2 }, positionNode [ 0, 0 ] [] { x = 3, y = 4 } ] ]) Nothing
            === (Ok "[(1, 2), (3, 4)]")
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
