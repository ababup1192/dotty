module DotsParser.Ast exposing (..)

import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Mouse exposing (Position)
import Debug


type alias Index =
    Int


type alias Id =
    List Index


type NodeData
    = NPosition Position
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


ast2Positions : Ast -> List Position
ast2Positions ast =
    let
        mzipper =
            ast
                |> ast2MZipper

        listMZipper =
            mzipper
                &> Zipper.goToChild 0

        positions =
            case (mzipper2MNForest listMZipper) of
                Just children ->
                    List.foldl
                        (\ast acc ->
                            case node2NData ast of
                                NPosition position ->
                                    position :: acc

                                _ ->
                                    acc
                        )
                        []
                        children

                Nothing ->
                    []
    in
        List.reverse positions


rootNode : NForest -> Ast
rootNode children =
    Tree { id = [], data = NRoot } children


listNode : Id -> NForest -> Ast
listNode id children =
    Tree { id = id, data = NList } children


positionNode : Id -> NForest -> Position -> Ast
positionNode id children position =
    Tree { id = id, data = NPosition position } children


data : Ast -> NodeData
data ast =
    case ast of
        Tree nodeDataId _ ->
            nodeDataId.data


updatePositionHelper : Position -> NodeDataWithId -> NodeDataWithId
updatePositionHelper position n =
    { n | data = NPosition position }


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen


getPosition : Id -> Ast -> Maybe Position
getPosition id ast =
    let
        mzipper =
            ast
                |> ast2MZipper
                |> applyCommands (toWalkCommands id)

        mposition =
            mzipper2MNodeDataId mzipper
                |> Maybe.map .data
                |> Maybe.andThen
                    (\data ->
                        case data of
                            NPosition position ->
                                Just position

                            _ ->
                                Nothing
                    )
    in
        mposition


insertPosition : Position -> Ast -> Ast
insertPosition position ast =
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
                &> Zipper.appendChild (positionNode newId [] position)
                &> Zipper.goToRoot
    in
        Maybe.withDefault ast <| mzipper2Ast insertedMZipper


updatePosition : Id -> Position -> Ast -> Ast
updatePosition id position ast =
    let
        mzipper =
            ast
                |> ast2MZipper
                |> applyCommands (toWalkCommands id)
    in
        let
            updatedMZipper =
                mzipper
                    &> Zipper.updateDatum (updatePositionHelper position)
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
