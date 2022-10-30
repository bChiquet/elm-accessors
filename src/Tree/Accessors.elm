module Tree.Accessors exposing (each, labelAt, onLabel, onPath)

import Base exposing (Lens, Optic)
import Tree exposing (Tree)
import Tree.Extra.Lue as Tree
import TreePath exposing (TreePath)


onLabel : Lens (Tree label) transformed label wrap
onLabel =
    Base.makeOneToOne "-label"
        Tree.label
        Tree.mapLabel


each :
    Optic label reachable wrap
    -> Optic (Tree label) reachable (Tree wrap)
each =
    Base.makeOneToN
        "<>"
        Tree.map
        Tree.map


onPath : TreePath -> Lens (Tree a) (Tree a) (Tree a) reachable
onPath path =
    Base.makeOneToN
        ("<" ++ String.join ", " (List.map String.fromInt path) ++ ">")
        (Tree.updateAt path)
        (Tree.updateAt path)


labelAt : TreePath -> Lens (Tree label) (Tree label) label reachable
labelAt path =
    onPath path << onLabel
