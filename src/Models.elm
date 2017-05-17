module Models exposing (..)

type alias Point = (Int, Int)

type alias Model =
    { code : String
    , points : List Point 
    }

initialModel : Model
initialModel = { code = "[]"
               , points = [] 
               }