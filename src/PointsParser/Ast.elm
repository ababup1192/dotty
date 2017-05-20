module PointsParser.Ast exposing (..)


--type Ast
--    = NPoint Int Int
--    | NList (List Ast)
--    | Root Ast


type alias Id = Int


type alias NodeInfo = {id: Id}


type Ast
    = NPoint {info: NodeInfo, x: Int, y: Int}
    | NList {info: NodeInfo, asts: List Ast}
    | Root {info: NodeInfo, ast: Ast}


showAst : Ast -> String
showAst ast =
    case ast of
        NPoint {x, y} ->
            "NPoint " ++ toString x ++ " " ++ toString y

        NList {asts} ->
            "NList [" ++ List.foldl (\a acc -> acc ++ "(" ++ showAst a ++ "),") "" asts ++ "]"

        Root {ast} ->
            "Root (" ++ showAst ast ++ ")"
