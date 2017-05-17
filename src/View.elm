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

view : Model -> Html Msg
view ( { code, points } as model) =
    div [ Attr.class "hybridEditor"
        ] 
    [ textEditor
    , visualEditor points
    ]

textEditor : Html Msg
textEditor = 
    div [ Attr.id "editor",
          Attr.class "editor"
        ] 
    []

visualEditor : List Models.Point -> Html Msg
visualEditor points = 
    svg [ SvgAttr.viewBox "0 0 450 450"
        , SvgAttr.class "visualEditor"
        , onCanvasClick
        ] 
        <| drawDots points

onCanvasClick : Svg.Attribute Msg
onCanvasClick =
  SvgEvent.on "click" (Decode.map Msg.CanvasClick Mouse.position)

drawDots : List Models.Point -> List (Svg msg)
drawDots =
    List.map
    (\point -> 
        let
            (x, y) = point
            cx = toString <| x - 470
            cy = toString <| y - 45
        in
            circle [ SvgAttr.cx cx, SvgAttr.cy cy
                   , SvgAttr.r "3", SvgAttr.fill "#0B79CE" 
                   ] 
                   []
    )

