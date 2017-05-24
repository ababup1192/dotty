module Tests exposing (..)

import Test exposing (..)
import ParserTest


-- Test modules


all : Test
all =
    describe "All Test" <|
        [ ParserTest.all
        ]
