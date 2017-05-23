module Update exposing (..)

import Models exposing (Model)
import Messages as Msg exposing (Msg)
import AceCodeBox
import DotsParser.Ast as Ast exposing (Ast)
import DotsParser.Parser as P
import DotsParser.Unparser as Unparser
import AppConstant
import Mouse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            updateCode model aceCodeBoxInfo

        Msg.CanvasClick position ->
            canvasClick model position

        Msg.DragStart xy id ->
            dragStart model xy id

        Msg.DragAt xy ->
            dragAt model xy

        Msg.DragEnd _ ->
            case model.drag of
                Just ({ target } as drag) ->
                    let
                        mPosition =
                            Ast.getPosition target model.ast

                        newAst =
                            case (getRealPosition model.ast model.drag mPosition) of
                                Just position ->
                                    Ast.updatePosition target position model.ast

                                Nothing ->
                                    Debug.crash "Can not found mouse position"
                    in
                        ( { model
                            | drag = Nothing
                            , ast = newAst
                            , code = Result.withDefault model.code <| Unparser.unparse newAst
                          }
                        , Cmd.none
                        )

                Nothing ->
                    Debug.crash "Drag target is not found."


updateCode : Model -> AceCodeBox.AceCodeBoxInfo -> ( Model, Cmd Msg )
updateCode ({ ast } as model) aceCodeBoxInfo =
    let
        newAst =
            Result.withDefault ast <| P.parse aceCodeBoxInfo.code
    in
        ( { model | code = aceCodeBoxInfo.code, ast = newAst, drag = Nothing }
        , Cmd.none
        )


canvasClick : Model -> Mouse.Position -> ( Model, Cmd Msg )
canvasClick ({ ast, drag } as model) position =
    let
        newPosition =
            { x = position.x - AppConstant.diffX, y = position.y - AppConstant.diffY }

        newAst =
            Ast.insertPosition newPosition ast

        newCode =
            Result.withDefault model.code (Unparser.unparse newAst)

        newModel =
            { model
                | ast = newAst
                , code = newCode
            }
    in
        case drag of
            Just _ ->
                ( newModel, Cmd.none )

            Nothing ->
                ( newModel
                , AceCodeBox.displayCode newModel
                )


dragStart : Model -> Mouse.Position -> Ast.Id -> ( Model, Cmd Msg )
dragStart model xy id =
    ( { model
        | drag = Just <| Models.Drag xy xy id
      }
    , Cmd.none
    )


dragAt : Model -> Mouse.Position -> ( Model, Cmd Msg )
dragAt ({ drag, ast, code } as model) xy =
    let
        newDrag =
            Maybe.map (\{ start, target } -> Models.Drag start xy target) drag

        currentPosition =
            case (Maybe.andThen (\d -> Ast.getPosition d.target ast) newDrag) of
                Just position ->
                    position

                Nothing ->
                    Debug.crash "can not get realPosition"

        targetId =
            case newDrag of
                Just { target } ->
                    target

                Nothing ->
                    Debug.crash "can not found drag"

        newAst =
            Ast.updatePosition targetId currentPosition ast

        newCode =
            Result.withDefault code <| Unparser.unparse newAst
    in
        ( { model
            | drag = newDrag
            , ast = newAst
            , code = newCode
          }
        , Cmd.none
        )


dragEnd : Model -> ( Model, Cmd Msg )
dragEnd ({ ast, drag } as model) =
    let
        targetId =
            case drag of
                Just { target } ->
                    target

                Nothing ->
                    Debug.crash "Drag target is not found."

        mPosition =
            Ast.getPosition targetId ast

        newAst =
            case (getRealPosition ast drag mPosition) of
                Just position ->
                    Ast.updatePosition targetId position ast

                Nothing ->
                    Debug.crash "Can not found mouse position"

        newCode =
            Result.withDefault model.code <| Unparser.unparse newAst
    in
        ( { model
            | drag = Nothing
            , ast = newAst
            , code = newCode
          }
        , Cmd.none
        )


getRealPosition : Ast -> Maybe Models.Drag -> Maybe Mouse.Position -> Maybe Mouse.Position
getRealPosition ast drag mPosition =
    case drag of
        Just { start, current, target } ->
            case mPosition of
                Just position ->
                    Just <|
                        Mouse.Position
                            (position.x + current.x - start.x)
                            (position.y + current.y - start.y)

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
