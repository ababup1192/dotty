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


getAst : Maybe ZipperAst -> Ast
getAst zipper =
    case zipper of
        Just ( ast, _ ) ->
            ast

        _ ->
            Debug.crash "getAst is Faild"


getNode : Maybe ZipperAst -> Maybe NodeDataWithId
getNode zipper =
    let
        (Tree nodeDataId _) =
            getAst zipper
    in
        Just nodeDataId


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
            getNode lastChild
                |> Maybe.map .id
                |> Maybe.andThen updateId
                |> Maybe.withDefault []

        insertedMZipper =
            listNode
                &> Zipper.appendChild (pointNode newId [] point)
                &> Zipper.goToRoot
    in
        getAst insertedMZipper


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
            getAst updatedMZipper


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
