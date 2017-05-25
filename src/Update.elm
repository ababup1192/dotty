module Update exposing (..)

import Models exposing (Model)
import Messages as Msg exposing (Msg)
import AceCodeBox
import DotsParser.Ast as Ast exposing (Ast)
import DotsParser.Parser as P
import DotsParser.Unparser as Unparser
import AppConstant
import Mouse
import Util


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            updateCode model aceCodeBoxInfo

        Msg.CanvasClick position ->
            canvasClick model position

        Msg.DragStart xy id ->
            dragStart model xy id

        Msg.DragAt drag xy ->
            dragAt drag model xy

        Msg.DragEnd drag _ ->
            dragEnd drag model


updateCode : Model -> AceCodeBox.AceCodeBoxInfo -> ( Model, Cmd Msg )
updateCode ({ ast, mdrag } as model) aceCodeBoxInfo =
    case mdrag of
        Just _ ->
            ( { model | code = aceCodeBoxInfo.code }
            , Cmd.none
            )

        Nothing ->
            let
                newAst =
                    Result.withDefault ast <| P.parse aceCodeBoxInfo.code
            in
                ( { model | code = aceCodeBoxInfo.code, ast = newAst }, Cmd.none )


canvasClick : Model -> Mouse.Position -> ( Model, Cmd Msg )
canvasClick ({ code, ast, mdrag } as model) position =
    case mdrag of
        Just _ ->
            ( model, Cmd.none )

        Nothing ->
            let
                newPosition =
                    { x = position.x - AppConstant.diffX, y = position.y - AppConstant.diffY }

                newAst =
                    Ast.insertPosition newPosition ast

                newCode =
                    Result.withDefault code (Unparser.unparse newAst Nothing)

                newModel =
                    { model
                        | ast = newAst
                        , code = newCode
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel
                )


dragStart : Model -> Mouse.Position -> Ast.Id -> ( Model, Cmd Msg )
dragStart model xy id =
    ( { model
        | mdrag = Just <| Models.Drag xy xy id
      }
    , Cmd.none
    )


dragAt : Models.Drag -> Model -> Mouse.Position -> ( Model, Cmd Msg )
dragAt drag ({ ast, code } as model) xy =
    let
        currentDrag =
            { drag | current = xy }

        targetPosition =
            Ast.getPosition currentDrag.targetId ast
    in
        case targetPosition of
            Just position ->
                let
                    newAst =
                        Ast.updatePosition currentDrag.targetId position ast

                    newCode =
                        Result.withDefault code <| Unparser.unparse newAst <| Just currentDrag

                    newModel =
                        { model
                            | mdrag = Just currentDrag
                            , ast = newAst
                            , code = newCode
                        }
                in
                    ( newModel
                    , AceCodeBox.displayCode newModel
                    )

            Nothing ->
                Debug.crash "can not get position"


dragEnd : Models.Drag -> Model -> ( Model, Cmd Msg )
dragEnd drag ({ ast, code } as model) =
    let
        mPosition =
            Ast.getPosition drag.targetId ast

        newAst =
            case mPosition of
                Just position ->
                    Ast.updatePosition drag.targetId (Util.getRealPosition drag position) ast

                Nothing ->
                    Debug.crash "Can not get mouse position"

        newCode =
            Result.withDefault code <| Unparser.unparse newAst Nothing

        newModel =
            { model
                | mdrag = Nothing
                , ast = newAst
                , code = newCode
            }
    in
        ( newModel
        , Cmd.none
        )
