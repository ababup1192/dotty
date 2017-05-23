module Messages exposing (..)

import AceCodeBox
import Mouse exposing (Position)
import DotsParser.Ast as Ast


type Msg
    = UpdateCode AceCodeBox.AceCodeBoxInfo
    | CanvasClick Mouse.Position
    | DragStart Position Ast.Id
    | DragAt Position
    | DragEnd Position
