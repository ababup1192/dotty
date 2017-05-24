module UpdateTest exposing (all)

import Test exposing (..)
import TestExp exposing (..)
import Update exposing (..)
import DotsParser.Ast as Ast
import Models exposing (Drag)


all : Test
all =
    describe "Update module Test" <|
        [ updateCodeTest
        ]


updateCodeTest : Test
updateCodeTest =
    describe "Update.updateCode Test" <|
        [ "updateCode []"
            => Tuple.first
                (updateCode { code = "", ast = Ast.rootNode [], drag = Nothing } { code = "[]" })
            === { code = "[]", ast = Ast.rootNode [ Ast.listNode [ 0 ] [] ], drag = Nothing }
        , "updateCode [(1, 2), (3, 4)]"
            => Tuple.first
                (updateCode { code = "[]", ast = Ast.rootNode [], drag = Nothing } { code = "[(1, 2), (3, 4)]" })
            === { code = "[(1, 2), (3, 4)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 1, y = 2 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 3, y = 4 }
                            ]
                        ]
                , drag = Nothing
                }
        , "updateCode [(1, 2), (102, 103)] with Drag"
            => Tuple.first
                (updateCode
                    { code = "[(1, 2), (102, 103)]"
                    , ast =
                        Ast.rootNode
                            [ Ast.listNode [ 0 ]
                                [ Ast.positionNode [ 0, 0 ] [] { x = 1, y = 2 }
                                , Ast.positionNode [ 0, 1 ] [] { x = 3, y = 4 }
                                ]
                            ]
                    , drag = Just <| Drag { x = 3, y = 4 } { x = 102, y = 103 } [ 0, 1 ]
                    }
                    { code = "[(1, 2), (102, 103)]" }
                )
            === { code = "[(1, 2), (102, 103)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 1, y = 2 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 3, y = 4 }
                            ]
                        ]
                , drag = Just <| Drag { x = 3, y = 4 } { x = 102, y = 103 } [ 0, 1 ]
                }
        ]
