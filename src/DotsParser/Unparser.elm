module DotsParser.Unparser exposing (..)

import DotsParser.Ast exposing (Ast, NodeData(NPosition, NList, NRoot))
import MultiwayTree as MT


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
        NPosition { x, y } ->
            let
                x_ =
                    toString x

                y_ =
                    toString y
            in
                Ok <| "(" ++ x_ ++ ", " ++ y_ ++ ")"

        _ ->
            Err "Here must be NPoint"
