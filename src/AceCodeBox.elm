port module AceCodeBox exposing
        ( initializeAndDisplay,
          receiveEditorState,
          AceCodeBoxInfo
        )

port aceCodeBoxCmd : AceCodeBoxCmd -> Cmd msg

type alias AceCodeBoxCmd = { message : String }
type alias AceCodeBoxInfo = { code: String }

initializeAndDisplay : a -> Cmd msg
initializeAndDisplay = sendCmd "initializeAndDisplay"

sendCmd : String -> a -> Cmd msg
sendCmd message model =
    aceCodeBoxCmd <| { message = message }

port receiveEditorState : (AceCodeBoxInfo -> msg) -> Sub msg