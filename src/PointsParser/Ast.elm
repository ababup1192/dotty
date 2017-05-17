module PointsParser.Ast exposing(..)

type Ast
  = NPoint Int Int
  | NList (List Ast)
  | Root Ast


showAst : Ast -> String
showAst ast =
  case ast of
    NPoint x y -> "NPoint " ++ toString x ++ " " ++ toString y

    NList asts -> "NList [" ++ List.foldl (\a acc -> acc ++ "(" ++ showAst a ++ "),") "" asts ++ "]"

    Root ast -> "Root (" ++ showAst ast ++ ")"