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
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel
                )

        Msg.DragStart xy id ->
            ( { model
                | drag = Just <| Models.Drag xy xy id
              }
            , Cmd.none
            )

        Msg.DragAt xy ->
            let
                newDrag =
                    Maybe.map (\{ start, target } -> Models.Drag start xy target) model.drag

                realPosition =
                    getRealPosition model.ast newDrag (Maybe.andThen (\d -> Ast.getPosition d.target model.ast) newDrag)
            in
                case realPosition of
                    Just position ->
                        case newDrag of
                            Just { target } ->
                                ( { model
                                    | drag = newDrag
                                    , ast = Ast.updatePosition target position model.ast
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                Debug.crash "can not found drag"

                    Nothing ->
                        Debug.crash "can not get realPosition"

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
                          }
                        , Cmd.none
                        )

                Nothing ->
                    Debug.crash "Drag target is not found."


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
