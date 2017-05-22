module Update exposing (..)

import Models exposing (Model)
import Messages as Msg exposing (Msg)
import AceCodeBox
import DotsParser.Ast as Ast
import DotsParser.Parser as P
import DotsParser.Unparser as Unparser
import AppConstant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            let
                newAst =
                    Result.withDefault model.ast <| P.parse aceCodeBoxInfo.code
            in
                ( { model | code = aceCodeBoxInfo.code, ast = newAst, drag = Nothing }, Cmd.none )

        Msg.CanvasClick position ->
            let
                { x, y } =
                    position

                newPosition =
                    { x = x - AppConstant.diffX, y = y - AppConstant.diffY }

                newAst =
                    Ast.insertPosition newPosition model.ast

                newCode =
                    Result.withDefault model.code (Unparser.unparse newAst)

                newModel =
                    { model
                        | ast = newAst
                        , code = newCode
                        , drag = Nothing
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel
                )

        Msg.DragStart xy id ->
            { model
                | drag = Just <| Models.Drag xy xy id
            }

        Msg.DragAt xy ->
            let
                newDrag =
                    Maybe.map (\{ start, id } -> Models.Drag start xy id) model.drag
            in
                { model
                    | drag = newDrag
                }
