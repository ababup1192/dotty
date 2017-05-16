module Update exposing (..)

import Models exposing (Model)
import Messages exposing (..)

-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode aceCodeBoxInfo ->
            ( { model | code = aceCodeBoxInfo.code }, Cmd.none )

