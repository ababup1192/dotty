module Update exposing (..)

import Models exposing (Model, Point)
import Messages as Msg exposing (Msg)
import AceCodeBox
import PointsParser.Ast exposing (Ast(NList, NPoint, Root))
import PointsParser.Parser exposing (parse)
import AppConstant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UpdateCode aceCodeBoxInfo ->
            ( { model | code = aceCodeBoxInfo.code, points = parse2points model.points aceCodeBoxInfo.code }, Cmd.none )

        Msg.CanvasClick position ->
            let
                { x, y } =
                    position

                newPosition =
                    [ ( x - AppConstant.diffX, y - AppConstant.diffY ) ]

                newPoints =
                    model.points ++ newPosition

                newModel =
                    { model
                        | points = newPoints
                        , code = toString newPoints
                    }
            in
                ( newModel
                , AceCodeBox.displayCode newModel
                )


parse2points : List Point -> String -> List Point
parse2points prevPoints code =
    let
        parseResult =
            Result.mapError (\_ -> ()) <| parse code

        resultValue =
            Result.andThen ast2value parseResult
    in
        case resultValue of
            Ok points ->
                points

            Err _ ->
                prevPoints


ast2value : Ast -> Result () (List Point)
ast2value ast =
    case ast of
        Root root ->
            nlist2value root.ast

        _ ->
            Err ()


nlist2value : Ast -> Result () (List Point)
nlist2value ast =
    case ast of
        NList nlist ->
            let
                f npoint acc =
                    case npoint2value npoint of
                        Ok point ->
                            Result.map (\acc_ -> point :: acc_) acc

                        _ ->
                            Err ()
            in
                List.foldl f (Ok []) nlist.asts

        _ ->
            Err ()


npoint2value : Ast -> Result () Point
npoint2value ast =
    case ast of
        NPoint point ->
            Ok ( point.x, point.y )

        _ ->
            Err ()
