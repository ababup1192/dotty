module PointsParser.Ast exposing (..)

import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Debug


type alias Index =
    Int


type alias Id =
    List Index


type alias Point =
    { x : Int, y : Int }


type NodeData
    = NPoint Point
    | NList
    | NRoot


type alias Identified a =
    { a | id : Id }


type alias NodeDataWithId =
    Identified { data : NodeData }


type alias Ast =
    Tree NodeDataWithId


type alias NForest =
    Forest NodeDataWithId


type alias ZipperAst =
    Zipper NodeDataWithId


initialAst : Ast
initialAst =
    Tree { id = [], data = NRoot } []


zipperelize : Ast -> ZipperAst
zipperelize ast =
    ( ast, [] )


rootNode : NForest -> Ast
rootNode children =
    Tree { id = [], data = NRoot } children


listNode : Id -> NForest -> Ast
listNode id children =
    Tree { id = id, data = NList } children


pointNode : Id -> NForest -> Point -> Ast
pointNode id children point =
    Tree { id = id, data = NPoint point } children


data : Ast -> NodeData
data ast =
    case ast of
        Tree info _ ->
            info.data


updatePoint : Point -> NodeDataWithId -> NodeDataWithId
updatePoint point n =
    { n | data = NPoint point }


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen



-- TODO: Refactor


updatePointAst : Id -> Point -> Ast -> Ast
updatePointAst id point ast =
    let
        mzipper =
            ast
                |> zipperelize
                |> Just
                |> applyCommands (toWalkCommands id)
    in
        let
            updatedMZipper =
                mzipper
                    &> Zipper.updateDatum (updatePoint point)
                    &> Zipper.goToRoot
        in
            let
                ( updatedAst, _ ) =
                    Maybe.withDefault ( ast, [] ) updatedMZipper
            in
                updatedAst


toWalkCommands : Id -> List (ZipperAst -> Maybe ZipperAst)
toWalkCommands id =
    List.map Zipper.goToChild id


applyCommands : List (ZipperAst -> Maybe ZipperAst) -> Maybe ZipperAst -> Maybe ZipperAst
applyCommands commands mzipper =
    List.foldl (\cmd acc -> Maybe.andThen cmd acc) mzipper commands
