module Main exposing (..)

import Html exposing (program)

import Models exposing (initialModel, Model)
import Messages as Msg exposing (Msg)
import View exposing (view)
import Update exposing (update)
import AceCodeBox


init : ( Model, Cmd Msg )
init =
    ( initialModel, initCmd )

initCmd : Cmd Msg
initCmd = 
    Cmd.batch
    [ AceCodeBox.initializeAndDisplay initialModel
    ]

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ AceCodeBox.receiveEditorState msgAceUpdate
    ]

msgAceUpdate : AceCodeBox.AceCodeBoxInfo -> Msg
msgAceUpdate aceCodeBoxInfo = Msg.UpdateCode aceCodeBoxInfo

-- Main
main : Program Never Model Msg
main =
    program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
