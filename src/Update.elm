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
            dragEnd model


updateCode : Model -> AceCodeBox.AceCodeBoxInfo -> ( Model, Cmd Msg )
updateCode ({ ast, drag } as model) aceCodeBoxInfo =
    case drag of
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
canvasClick ({ code, ast, drag } as model) position =
    case drag of
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
        | drag = Just <| Models.Drag xy xy id
      }
    , Cmd.none
    )


dragAt : Model -> Mouse.Position -> ( Model, Cmd Msg )
dragAt ({ drag, ast, code } as model) xy =
    let
        newDrag =
            Maybe.map (\{ start, target } -> Models.Drag start xy target) drag

        realPosition =
            Maybe.andThen (\d -> Ast.getPosition d.target ast) newDrag
    in
        case realPosition of
            Just position ->
                case newDrag of
                    Just { target } ->
                        let
                            newAst =
                                Ast.updatePosition target position ast

                            newCode =
                                Result.withDefault code <| Unparser.unparse newAst newDrag

                            newModel =
                                { model
                                    | drag = newDrag
                                    , ast = newAst
                                    , code = newCode
                                }
                        in
                            ( newModel
                            , AceCodeBox.displayCode newModel
                            )

                    Nothing ->
                        Debug.crash "can not found drag"

            Nothing ->
                Debug.crash "can not get realPosition"


dragEnd : Model -> ( Model, Cmd Msg )
dragEnd ({ ast, drag, code } as model) =
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
            case (getRealPosition drag mPosition) of
                Just position ->
                    Ast.updatePosition targetId position ast

                Nothing ->
                    Debug.crash "Can not found mouse position"

        newCode =
            Result.withDefault code <| Unparser.unparse newAst Nothing

        newModel =
            { model
                | drag = Nothing
                , ast = newAst
                , code = newCode
            }
    in
        ( newModel
        , AceCodeBox.displayCode newModel
        )


getRealPosition : Maybe Models.Drag -> Maybe Mouse.Position -> Maybe Mouse.Position
getRealPosition drag mPosition =
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
