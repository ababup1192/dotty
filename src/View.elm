module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Svg exposing (..)
import Svg.Attributes as SvgAttr

import Messages exposing (..)
import Models exposing (Model)

-- View
view : Model -> Html Msg
view ( { code } as model) =
    div [ Attr.class "hybridEditor"
        ] 
     [
        textEditor,
        visualEditor
    ]

textEditor : Html Msg
textEditor = 
    div [ Attr.id "editor"
        , Attr.style [ ("position", "relative")
            , ("width",  pixels 450)
            , ("height", pixels 500)
          ]
        ] 
    [ ]

visualEditor : Html Msg
visualEditor = 
    svg [ SvgAttr.viewBox "0 0 100 100"
        , SvgAttr.width <| pixels 350
        , SvgAttr.class "visualEditor"
        ]
        [ circle [ SvgAttr.cx "50", SvgAttr.cy "50", 
            SvgAttr.r "3", SvgAttr.fill "#0B79CE" ] []
        ]



pixels : Int -> String
pixels n = toString n ++ "px"