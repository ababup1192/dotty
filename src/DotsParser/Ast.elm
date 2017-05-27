module DotsParser.Ast exposing (..)

import Mouse exposing (Position)
import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)


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


type alias PositionWithId =
    { id : Id, position : Position }


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
mzipper2Ast =
    Maybe.map Tuple.first


node2Id : Ast -> Id
node2Id ast =
    MultiwayTree.datum ast |> .id


node2NData : Ast -> NodeData
node2NData ast =
    MultiwayTree.datum ast |> .data


mzipper2MNForest : Maybe ZipperAst -> Maybe NForest
mzipper2MNForest =
    Maybe.map MultiwayTree.children << mzipper2Ast


listMZipper2ListPositionWithId : Maybe ZipperAst -> List PositionWithId
listMZipper2ListPositionWithId listMZipper =
    List.reverse <|
        case mzipper2MNForest listMZipper of
            Just children ->
                List.foldl
                    (\ast acc ->
                        case node2NData ast of
                            NPosition position ->
                                { id = (MultiwayTree.datum ast).id
                                , position = position
                                }
                                    :: acc

                            _ ->
                                acc
                    )
                    []
                    children

            Nothing ->
                []


ast2Positions : Ast -> List PositionWithId
ast2Positions ast =
    let
        mzipper =
            ast
                |> ast2MZipper

        listMZipper =
            mzipper
                &> Zipper.goToChild 0
    in
    listMZipper2ListPositionWithId listMZipper


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


getChildren : Ast -> List Ast
getChildren ast =
    case ast of
        Tree _ cld ->
            cld


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
            mzipper2Ast mzipper
                |> Maybe.map node2NData
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
            mzipper2Ast lastChild
                |> Maybe.map node2Id
                |> Maybe.andThen updateId
                |> Maybe.withDefault [ 0, 0 ]

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


deletePosition : Id -> Ast -> Ast
deletePosition id ast =
    let
        mzipper =
            ast2MZipper ast

        parentId =
            List.take (List.length id - 1) id

        parentCommand =
            toWalkCommands <| parentId

        assignIdIfNPoint index ast =
            case data ast of
                NPosition position ->
                    positionNode (List.reverse <| index :: parentId) [] position

                _ ->
                    ast

        assignId asts =
            List.foldl (\ast ( acc, index ) -> ( assignIdIfNPoint index ast :: acc, index + 1 )) ( [], 0 ) asts

        oldChildren =
            Maybe.map (\( ast, _ ) -> getChildren ast) <| applyCommands parentCommand <| ast2MZipper ast

        delete =
            List.filter
                (\ast ->
                    case ast of
                        Tree nodeId _ ->
                            nodeId.id /= id
                )

        newChildren =
            Maybe.map (List.reverse << Tuple.first << assignId << delete) oldChildren
    in
    Maybe.withDefault ast <| Maybe.map (\children -> rootNode [ listNode [ 0 ] children ]) newChildren


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
