module PointsParser.Unparser exposing (..)

import PointsParser.Ast exposing (Ast, NodeData(NPoint, NList, NRoot))
import MultiwayTree as MT


type alias Point =
    ( Int, Int )


type alias ParsedValue =
    List Point



-- unparse : ParsedValue -> Result String String
-- unparse =
--    unparseAst << toAst
-- toAst : ParsedValue -> Ast
-- toAst points =
--    let
--        toNDot ( x, y ) =
--            NPoint x y
--    in
--        Root <| NList <| List.map toNDot points


unparse : Ast -> Result String String
unparse (MT.Tree { id, data } children) =
    case data of
        NRoot ->
            case children of
                [ l ] ->
                    list l

                _ ->
                    Err "Children must be One node."

        _ ->
            Err "Ast root must be Root."


list : Ast -> Result String String
list (MT.Tree { id, data } children) =
    case data of
        NList ->
            let
                concat acc p =
                    Result.map (\acc_ -> p ++ acc_) acc

                points =
                    List.map point children
                        |> List.foldl
                            (\p acc ->
                                case p of
                                    Ok p_ ->
                                        p_ :: acc

                                    Err _ ->
                                        acc
                            )
                            []
                        |> List.reverse
                        |> String.join ", "
            in
                Ok <| "[" ++ points ++ "]"

        _ ->
            Err "Here must be NList."


point : Ast -> Result String String
point (MT.Tree { id, data } children) =
    case data of
        NPoint { x, y } ->
            let
                x_ =
                    toString x

                y_ =
                    toString y
            in
                Ok <| "(" ++ x_ ++ ", " ++ y_ ++ ")"

        _ ->
            Err "Here must be NPoint"
