module Models exposing (..)

import PointsParser.Ast exposing (Ast, initialAst)


type alias Model =
    { code : String
    , ast : Ast
    }


initialModel : Model
initialModel =
    { code = "[]"
    , ast = initialAst
    }
