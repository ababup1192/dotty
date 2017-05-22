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
import PointsParser.Ast exposing (Ast, Point)


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
        drawDots ast


onCanvasClick : Svg.Attribute Msg
onCanvasClick =
    SvgEvent.on "click" (Decode.map Msg.CanvasClick Mouse.position)


drawDots : Ast -> List (Svg msg)
drawDots =
    List.map
        (\point ->
            let
                ( x, y ) =
                    point

                cx =
                    toString <| x - AppConstant.viewDiffX

                cy =
                    toString <| y - AppConstant.viewDiffY
            in
                circle
                    [ SvgAttr.cx cx
                    , SvgAttr.cy cy
                    , SvgAttr.r "3"
                    , SvgAttr.fill "#0B79CE"
                    ]
                    []
        )
