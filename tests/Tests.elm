module Tests exposing (..)

import Test exposing (..)
import ParserTest
import UpdateTest


-- Test modules


all : Test
all =
    describe "All Test" <|
        [ ParserTest.all
        , UpdateTest.all
        ]
