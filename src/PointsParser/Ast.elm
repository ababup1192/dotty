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


ast2MZipper : Ast -> Maybe ZipperAst
ast2MZipper ast =
    Just ( ast, [] )


mzipper2Ast : Maybe ZipperAst -> Maybe Ast
mzipper2Ast zipper =
    case zipper of
        Just ( ast, _ ) ->
            Just ast

        _ ->
            Nothing


mzipper2MNodeDataId : Maybe ZipperAst -> Maybe NodeDataWithId
mzipper2MNodeDataId mzipper =
    mzipper2Ast mzipper
        |> Maybe.map (\(Tree nodeDataId _) -> nodeDataId)


node2NData : Ast -> NodeData
node2NData (Tree nodeDataId _) =
    nodeDataId.data


mzipper2MNForest : Maybe ZipperAst -> Maybe NForest
mzipper2MNForest mzipper =
    mzipper2Ast mzipper
        |> Maybe.map (\(Tree _ children) -> children)


ast2Points : Ast -> List Point
ast2Points ast =
    let
        mzipper =
            ast
                |> ast2MZipper

        listMZipper =
            mzipper
                &> Zipper.goToChild 0

        points =
            case (mzipper2MNForest listMZipper) of
                Just children ->
                    List.foldl
                        (\ast acc ->
                            case node2NData ast of
                                NPoint point ->
                                    point :: acc

                                _ ->
                                    acc
                        )
                        []
                        children

                Nothing ->
                    []
    in
        List.reverse points


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
        Tree nodeDataId _ ->
            nodeDataId.data


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
        mzipper =
            ast
                |> ast2MZipper

        listMZipper =
            mzipper
                &> Zipper.goToChild 0

        lastChild =
            listMZipper
                &> Zipper.goToRightMostChild

        newId =
            mzipper2MNodeDataId lastChild
                |> Maybe.map .id
                |> Maybe.andThen updateId
                |> Maybe.withDefault []

        insertedMZipper =
            listMZipper
                &> Zipper.appendChild (pointNode newId [] point)
                &> Zipper.goToRoot
    in
        Maybe.withDefault ast <| mzipper2Ast insertedMZipper


updatePoint : Id -> Point -> Ast -> Ast
updatePoint id point ast =
    let
        mzipper =
            ast
                |> ast2MZipper
                |> applyCommands (toWalkCommands id)
    in
        let
            updatedMZipper =
                mzipper
                    &> Zipper.updateDatum (updatePointHelper point)
                    &> Zipper.goToRoot
        in
            Maybe.withDefault ast <| mzipper2Ast updatedMZipper


toWalkCommands : Id -> List (ZipperAst -> Maybe ZipperAst)
toWalkCommands id =
    List.map Zipper.goToChild id


applyCommands : List (ZipperAst -> Maybe ZipperAst) -> Maybe ZipperAst -> Maybe ZipperAst
applyCommands commands mzipper =
    List.foldl (\cmd acc -> Maybe.andThen cmd acc) mzipper commands


updateId : Id -> Maybe Id
updateId old =
    case List.reverse old of
        x :: xs ->
            Just <| xs ++ [ x + 1 ]

        _ ->
            Nothing
