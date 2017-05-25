module Messages exposing (..)

import AceCodeBox
import Mouse exposing (Position)
import DotsParser.Ast as Ast
import Models


type Msg
    = UpdateCode AceCodeBox.AceCodeBoxInfo
    | CanvasClick Mouse.Position
    | DragStart Position Ast.Id
    | DragAt Models.Drag Position
    | DragEnd Models.Drag Position
