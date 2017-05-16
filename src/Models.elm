module Models exposing (..)

-- Model
type alias Model =
    { code: String }

initialModel : Model
initialModel = { code = "" }