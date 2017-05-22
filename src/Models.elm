module Models exposing (..)

import DotsParser.Ast exposing (Ast, initialAst)


type alias Model =
    { code : String
    , ast : Ast
    }


initialModel : Model
initialModel =
    { code = "[]"
    , ast = initialAst
    }
