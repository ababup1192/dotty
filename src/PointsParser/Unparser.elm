module PointsParser.Unparser exposing (..)

import PointsParser.Ast exposing (Ast(NPoint, NList, Root))


type alias Point =
    ( Int, Int )


type alias ParsedValue =
    List Point


unparse : ParsedValue -> Result String String
unparse =
    unparseAst << toAst


toAst : ParsedValue -> Ast
toAst points =
    let
        toNDot ( x, y ) =
            NPoint x y
    in
        Root <| NList <| List.map toNDot points


unparseAst : Ast -> Result String String
unparseAst ast =
    case ast of
        Root n ->
            list n

        _ ->
            Err "Ast root must be Root."


list : Ast -> Result String String
list ast =
    case ast of
        NList npoints ->
            let
                concat acc p =
                    Result.map (\acc_ -> p ++ acc_) acc

                points =
                    List.foldl (\p acc -> Result.andThen (concat acc) p) (Ok "") <| List.map point npoints
            in
                Result.map (\points_ -> "[" ++ points_ ++ "]") points

        _ ->
            Err "Here must be NList."


point : Ast -> Result String String
point ast =
    case ast of
        NPoint x y ->
            let
                x_ =
                    toString x

                y_ =
                    toString y
            in
                Ok <| "(" ++ x_ ++ ", " ++ y_ ++ ")"

        _ ->
            Err "Here must be NPoint"
