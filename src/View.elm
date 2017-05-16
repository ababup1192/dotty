module View exposing (view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput)

import Messages exposing (..)
import Models exposing (Model)


-- View
view : Model -> Html Msg
view ( { code } as model) =
    div 
        [ Attr.id "editor"
        , Attr.style [ ("position", "relative")
          , ("width",  pixels 700)
          , ("height", pixels 500)
          ]
        ] 
    [ ]

pixels : Int -> String
pixels n = toString n ++ "px"