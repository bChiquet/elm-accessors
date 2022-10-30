module Tree.Accessors exposing (each, labelAt, onLabel, onPath)

import Base exposing (Optic)
import Tree exposing (Tree)
import Tree.Extra.Lue as Tree
import TreePath exposing (TreePath)


onLabel : Optic attr view attr -> Optic (Tree attr) view (Tree attr)
onLabel =
    Base.makeOneToOne "-label"
        Tree.label
        Tree.mapLabel


each : Optic attr view over -> Optic (Tree attr) (Tree view) (Tree over)
each =
    Base.makeOneToN
        "<>"
        Tree.map
        Tree.map


onPath :
    TreePath
    -> Optic (Tree attr) (Tree attr) (Tree attr)
    -> Optic (Tree attr) (Tree attr) (Tree attr)
onPath path =
    Base.makeOneToN
        ("<" ++ String.join ", " (List.map String.fromInt path) ++ ">")
        (Tree.updateAt path)
        (Tree.updateAt path)


labelAt :
    List Int
    -> Optic attr (Tree attr) attr
    -> Optic (Tree attr) (Tree attr) (Tree attr)
labelAt path =
    onPath path << onLabel
