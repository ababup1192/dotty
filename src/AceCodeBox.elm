port module AceCodeBox
    exposing
        ( initializeAndDisplay
        , displayCode
        , receiveEditorState
        , AceCodeBoxInfo
        )

import Models


type alias AceCodeBoxCmd =
    { message : String, code : String }


port aceCodeBoxCmd : AceCodeBoxCmd -> Cmd msg


type alias AceCodeBoxInfo =
    { code : String }


initializeAndDisplay : Models.Model -> Cmd msg
initializeAndDisplay =
    sendCmd "initializeAndDisplay"


displayCode : Models.Model -> Cmd msg
displayCode =
    sendCmd "displayCode"


sendCmd : String -> Models.Model -> Cmd msg
sendCmd message model =
    aceCodeBoxCmd { message = message, code = model.code }


port receiveEditorState : (AceCodeBoxInfo -> msg) -> Sub msg
