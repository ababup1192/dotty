module Main exposing (..)

import Html exposing (program)

import Models exposing (initialModel, Model)
import Messages exposing (Msg)
import View exposing (view)
import Update exposing (update)
import AceCodeBox


init : ( Model, Cmd Msg )
init =
    ( initialModel, initCmd )

initCmd : Cmd Msg
initCmd = 
    Cmd.batch
        [ AceCodeBox.initializeAndDisplay { message = "initial" }
        ]

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        AceCodeBox.receiveEditorState msgAceUpdate
    ]

msgAceUpdate : AceCodeBox.AceCodeBoxInfo -> Msg
msgAceUpdate aceCodeBoxInfo = Messages.UpdateCode aceCodeBoxInfo

-- Main
main : Program Never Model Msg
main =
    program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
