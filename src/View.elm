module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvent
import Mouse
import Json.Decode as Decode
import Messages as Msg exposing (Msg)
import Models exposing (Model)
import AppConstant
import DotsParser.Ast as Ast exposing (Ast)
import Mouse exposing (Position)


view : Model -> Html Msg
view ({ code, ast } as model) =
    div
        [ Attr.class "hybridEditor"
        ]
        [ textEditor
        , visualEditor ast
        ]


textEditor : Html Msg
textEditor =
    div
        [ Attr.id "textEditor"
        , Attr.class "textEditor"
        ]
        []


visualEditor : Ast -> Html Msg
visualEditor ast =
    svg
        [ SvgAttr.viewBox "0 0 450 450"
        , SvgAttr.class "visualEditor"
        , onCanvasClick
        ]
    <|
        drawDots <|
            Ast.ast2Positions ast


onCanvasClick : Svg.Attribute Msg
onCanvasClick =
    SvgEvent.on "click" (Decode.map Msg.CanvasClick Mouse.position)


drawDots : List Position -> List (Svg msg)
drawDots =
    List.map
        (\point ->
            let
                cx =
                    toString <| point.x - AppConstant.viewDiffX

                cy =
                    toString <| point.y - AppConstant.viewDiffY

                c =
                    circle
                        [ SvgAttr.cx cx
                        , SvgAttr.cy cy
                        , SvgAttr.r "3"
                        , SvgAttr.fill "#0B79CE"
                        ]
                        []
            in
                c
        )
