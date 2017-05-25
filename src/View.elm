module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvent
import Mouse
import Json.Decode as Decode
import Messages as Msg exposing (Msg)
import Models exposing (Drag, Model)
import AppConstant
import DotsParser.Ast as Ast exposing (Ast)
import Mouse exposing (Position)
import Util


view : Model -> Html Msg
view ({ code, ast, mdrag } as model) =
    div
        [ Attr.class "hybridEditor"
        ]
        [ textEditor
        , visualEditor ast mdrag
        ]


textEditor : Html Msg
textEditor =
    div
        [ Attr.id "textEditor"
        , Attr.class "textEditor"
        ]
        []


visualEditor : Ast -> Maybe Drag -> Html Msg
visualEditor ast mdrag =
    svg
        [ SvgAttr.viewBox "0 0 450 450"
        , SvgAttr.class "visualEditor"
        , onCanvasClick
        ]
    <|
        drawDots (Ast.ast2Positions ast) ast mdrag


drawDots : List Ast.PositionWithId -> Ast -> Maybe Drag -> List (Svg Msg)
drawDots nodes ast mdrag =
    List.map
        (\{ position, id } ->
            let
                xy =
                    case mdrag of
                        Just drag ->
                            if id == drag.targetId then
                                Util.getRealPosition drag position
                            else
                                position

                        _ ->
                            position

                cx =
                    toString <| xy.x - AppConstant.viewDiffX

                cy =
                    toString <| xy.y - AppConstant.viewDiffY

                c =
                    circle
                        [ onCircleMouseDown id
                        , SvgAttr.class "circle"
                        , SvgAttr.cx cx
                        , SvgAttr.cy cy
                        , SvgAttr.r "4"
                        , SvgAttr.fill "#0B79CE"
                        ]
                        []
            in
                c
        )
        nodes


onCanvasClick : Svg.Attribute Msg
onCanvasClick =
    SvgEvent.on "click" (Decode.map Msg.CanvasClick Mouse.position)


onCircleMouseDown : Ast.Id -> Svg.Attribute Msg
onCircleMouseDown target =
    SvgEvent.on "mousedown" (Decode.map (\position -> Msg.DragStart position target) Mouse.position)
