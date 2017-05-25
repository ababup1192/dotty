module Util exposing (..)

import Models
import Mouse


getRealPosition : Models.Drag -> Mouse.Position -> Mouse.Position
getRealPosition { start, current } position =
    Mouse.Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)
