module Messages exposing (..)

import AceCodeBox
import Mouse

type Msg = UpdateCode AceCodeBox.AceCodeBoxInfo
         | CanvasClick Mouse.Position

