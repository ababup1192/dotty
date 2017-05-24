module UpdateTest exposing (all)

import Test exposing (..)
import TestExp exposing (..)
import Update exposing (..)
import DotsParser.Ast as Ast


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
        ]
