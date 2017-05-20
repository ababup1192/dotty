module PointsParser.Ast exposing (..)

import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper exposing (Zipper)


type alias Index =
    Int


type alias Id =
    List Index


type NodeData
    = NPoint { x : Int, y : Int }
    | NList
    | NRoot


type alias Identified a =
    { a | id : Id }


type alias NodeInfo =
    Identified { data : NodeData }


type alias ZipperAst =
    Zipper NodeInfo


type alias Ast =
    Tree NodeInfo


initialAst : Ast
initialAst =
    Tree { id = [], data = NRoot } []


zipperelize : Ast -> Zipper NodeInfo
zipperelize ast =
    ( ast, [] )


rootNode : Forest NodeInfo -> Ast
rootNode children =
    Tree { id = [], data = NRoot } children


listNode : Id -> Forest NodeInfo -> Ast
listNode id children =
    Tree { id = id, data = NList } children


pointNode : Id -> Forest NodeInfo -> { x : Int, y : Int } -> Ast
pointNode id children point =
    Tree { id = id, data = NPoint point } children


data : Ast -> NodeData
data ast =
    case ast of
        Tree info _ ->
            info.data
