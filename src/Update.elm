module Update exposing (..)

import Models exposing (Model)
import Messages as Msg exposing (Msg)
import AceCodeBox

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            ( { model | code = aceCodeBoxInfo.code }, Cmd.none )

        Msg.CanvasClick position ->
            let 
                { x, y } = position
                newPoints = model.points ++ [(x, y)]
                newModel = { model 
                    | points = newPoints
                    , code = toString newPoints
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel )

