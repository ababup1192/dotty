module Models exposing (..)

import DotsParser.Ast as Ast exposing (Ast, initialAst)
import Mouse exposing (Position)


type alias Model =
    { code : String
    , ast : Ast
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    , target : Ast.Id
    }


initialModel : Model
initialModel =
    { code = "[]"
    , ast = initialAst
    , drag = Nothing
    }
