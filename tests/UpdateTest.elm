module UpdateTest exposing (all)

import Test exposing (..)
import TestExp exposing (..)
import Update exposing (..)
import DotsParser.Ast as Ast
import Models exposing (Drag)
import AppConstant


all : Test
all =
    describe "Update module Test" <|
        [ updateCodeTest
        , canvasClickTest
        , dragStartTest
        , dragAtTest
        , dragEndTest
        ]


updateCodeTest : Test
updateCodeTest =
    describe "Update.updateCode Test" <|
        [ "updateCode []"
            => Tuple.first
                (updateCode { code = "", ast = Ast.rootNode [], mdrag = Nothing } { code = "[]" })
            === { code = "[]", ast = Ast.rootNode [ Ast.listNode [ 0 ] [] ], mdrag = Nothing }
        , "updateCode [(1, 2), (3, 4)]"
            => Tuple.first
                (updateCode { code = "[]", ast = Ast.rootNode [], mdrag = Nothing } { code = "[(1, 2), (3, 4)]" })
            === { code = "[(1, 2), (3, 4)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 1, y = 2 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 3, y = 4 }
                            ]
                        ]
                , mdrag = Nothing
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
                    , mdrag = Just <| Drag { x = 3, y = 4 } { x = 102, y = 103 } [ 0, 1 ]
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
                , mdrag = Just <| Drag { x = 3, y = 4 } { x = 102, y = 103 } [ 0, 1 ]
                }
        ]


canvasClickTest : Test
canvasClickTest =
    describe "Update.canvasClick Test" <|
        [ let
            canvasX =
                500

            canvasY =
                50

            astX =
                canvasX - AppConstant.diffX

            astY =
                canvasY - AppConstant.diffY

            positionText =
                "(" ++ (toString astX) ++ ", " ++ (toString astY) ++ ")"
          in
            "canvasClick [] <| click (30, 30)"
                => Tuple.first
                    (canvasClick { code = "[]", ast = Ast.rootNode [ Ast.listNode [ 0 ] [] ], mdrag = Nothing } { x = canvasX, y = canvasY })
                === { code = "[(30, 30)]"
                    , ast =
                        Ast.rootNode
                            [ Ast.listNode [ 0 ]
                                [ Ast.positionNode [ 0, 0 ] [] { x = astX, y = astY }
                                ]
                            ]
                    , mdrag = Nothing
                    }
        , "canvasClick [(0, 0)] <| click (30, 30) with Drag"
            => Tuple.first
                (canvasClick
                    { code = "[(0, 0)]"
                    , ast = Ast.rootNode [ Ast.listNode [ 0 ] [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 } ] ]
                    , mdrag = Just <| Drag { x = 0, y = 0 } { x = 0, y = 0 } [ 0, 0 ]
                    }
                    { x = 0, y = 0 }
                )
            === { code = "[(0, 0)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                            ]
                        ]
                , mdrag = Just <| Drag { x = 0, y = 0 } { x = 0, y = 0 } [ 0, 0 ]
                }
        ]


dragStartTest : Test
dragStartTest =
    describe "Update.dragStart Test" <|
        [ "dragStart []"
            => Tuple.first
                (dragStart
                    { code = "[(0, 0), (10, 10)]"
                    , ast =
                        Ast.rootNode
                            [ Ast.listNode [ 0 ]
                                [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                                , Ast.positionNode [ 0, 1 ] [] { x = 10, y = 10 }
                                ]
                            ]
                    , mdrag = Nothing
                    }
                    { x = 10, y = 10 }
                    [ 0, 1 ]
                )
            === { code = "[(0, 0), (10, 10)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 10, y = 10 }
                            ]
                        ]
                , mdrag = Just <| Drag { x = 10, y = 10 } { x = 10, y = 10 } [ 0, 1 ]
                }
        ]


dragAtTest : Test
dragAtTest =
    describe "Update.dragAt Test" <|
        [ "dragAt [(0, 0), (5, 5)] <| drag (5, 6) [0, 1]"
            => Tuple.first
                (dragAt
                    (Drag { x = 5, y = 5 } { x = 5, y = 5 } [ 0, 1 ])
                    { code = "[(0, 0), (5, 5)]"
                    , ast =
                        Ast.rootNode
                            [ Ast.listNode [ 0 ]
                                [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                                , Ast.positionNode [ 0, 1 ] [] { x = 5, y = 5 }
                                ]
                            ]
                    , mdrag = Just <| Drag { x = 5, y = 5 } { x = 5, y = 5 } [ 0, 1 ]
                    }
                    { x = 5, y = 6 }
                )
            === { code = "[(0, 0), (5, 6)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 5, y = 5 }
                            ]
                        ]
                , mdrag = Just <| Drag { x = 5, y = 5 } { x = 5, y = 6 } [ 0, 1 ]
                }
        ]


dragEndTest : Test
dragEndTest =
    describe "Update.dragEnd Test" <|
        [ "dragEnd [(0, 0), (5, 5)] <| drag (10, 10) [0, 1]"
            => Tuple.first
                (dragEnd
                    (Drag { x = 5, y = 5 } { x = 10, y = 10 } [ 0, 1 ])
                    { code = "[(0, 0), (10, 10)]"
                    , ast =
                        Ast.rootNode
                            [ Ast.listNode [ 0 ]
                                [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                                , Ast.positionNode [ 0, 1 ] [] { x = 5, y = 5 }
                                ]
                            ]
                    , mdrag = Just <| Drag { x = 5, y = 5 } { x = 10, y = 10 } [ 0, 1 ]
                    }
                )
            === { code = "[(0, 0), (10, 10)]"
                , ast =
                    Ast.rootNode
                        [ Ast.listNode [ 0 ]
                            [ Ast.positionNode [ 0, 0 ] [] { x = 0, y = 0 }
                            , Ast.positionNode [ 0, 1 ] [] { x = 10, y = 10 }
                            ]
                        ]
                , mdrag = Nothing
                }
        ]
