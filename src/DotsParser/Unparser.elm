module DotsParser.Unparser exposing (..)

import DotsParser.Ast exposing (Ast, NodeData(NPosition, NList, NRoot))
import MultiwayTree as MT
import Models exposing (Drag)
import Mouse
import Util


unparse : Ast -> Maybe Drag -> Result String String
unparse (MT.Tree { id, data } children) mdrag =
    case data of
        NRoot ->
            case children of
                [ l ] ->
                    list l mdrag

                _ ->
                    Err "Children must be One node."

        _ ->
            Err "Ast root must be Root."


list : Ast -> Maybe Drag -> Result String String
list (MT.Tree { id, data } children) mdrag =
    case data of
        NList ->
            let
                concat acc p =
                    Result.map (\acc_ -> p ++ acc_) acc

                points =
                    List.map (\c -> point c mdrag) children
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


point : Ast -> Maybe Drag -> Result String String
point (MT.Tree { id, data } children) mdrag =
    case data of
        NPosition position ->
            let
                x_ =
                    toString <| position.x

                y_ =
                    toString <| position.y

                ps sx sy =
                    "(" ++ sx ++ ", " ++ sy ++ ")"
            in
                case mdrag of
                    Just ({ targetId } as drag) ->
                        if targetId == id then
                            pointHelper drag position
                        else
                            Ok <| ps x_ y_

                    Nothing ->
                        Ok <| ps x_ y_

        _ ->
            Err "Here must be NPoint"


pointHelper : Models.Drag -> Mouse.Position -> Result String String
pointHelper drag position =
    let
        realPosition =
            Util.getRealPosition drag position

        x =
            toString <| realPosition.x

        y =
            toString <| realPosition.y

        ps sx sy =
            "(" ++ sx ++ ", " ++ sy ++ ")"
    in
        Ok <| ps x y
