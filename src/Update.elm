module Update exposing (..)

import Models exposing (Model)
import Messages as Msg exposing (Msg)
import AceCodeBox
import PointsParser.Ast as Ast exposing (Ast(NList, NPoint, Root))
import PointsParser.Parser as P
import PointsParser.Unparser as Unparser
import AppConstant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            let
                newAst =
                    Result.withDefault model.ast <| P.parse aceCodeBoxInfo.code
            in
                ( { model | code = aceCodeBoxInfo.code, ast = newAst }, Cmd.none )

        Msg.CanvasClick position ->
            let
                { x, y } =
                    position

                newPosition =
                    { x = x - AppConstant.diffX, y = y - AppConstant.diffY }

                newAst =
                    Ast.insertPoint newPosition model.ast

                newCode =
                    Result.withDefault model.code Unparser.unparse newAst

                newModel =
                    { model
                        | ast = newAst
                        , code = newCode
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel
                )
