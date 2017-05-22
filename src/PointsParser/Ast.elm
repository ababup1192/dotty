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


zipperelize : Ast -> Maybe ZipperAst
zipperelize ast =
    Just ( ast, [] )


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


updatePointHelper : Point -> NodeDataWithId -> NodeDataWithId
updatePointHelper point n =
    { n | data = NPoint point }


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen



-- TODO: Refactor


insertPoint : Point -> Ast -> Ast
insertPoint point ast =
    let
        zipper =
            ast
                |> zipperelize

        listNode =
            zipper
                &> Zipper.goToChild 0

        lastChild =
            listNode
                &> Zipper.goToRightMostChild

        newId =
            Maybe.map Tuple.first lastChild
                |> Maybe.andThen
                    (\tree ->
                        case tree of
                            Tree node _ ->
                                List.head <| List.reverse node.id
                    )
                |> Maybe.withDefault -1
                |> (+) 1
                |> List.singleton
                |> (++) [ 0 ]

        insertedMZipper =
            listNode
                &> Zipper.appendChild (pointNode newId [] point)
                &> Zipper.goToRoot
    in
        let
            ( insertedAst, _ ) =
                Maybe.withDefault ( ast, [] ) insertedMZipper
        in
            insertedAst


updatePoint : Id -> Point -> Ast -> Ast
updatePoint id point ast =
    let
        mzipper =
            ast
                |> zipperelize
                |> applyCommands (toWalkCommands id)
    in
        let
            updatedMZipper =
                mzipper
                    &> Zipper.updateDatum (updatePointHelper point)
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
