module Tree.Accessors exposing
    ( at, label_, path
    , each
    )

{-| List.Accessors

@docs at, label_, path
@docs each

-}

import Base exposing (Lens, Optic, Traversal)
import Tree exposing (Tree)
import Tree.Extra.Lue as Tree
import TreePath exposing (TreePath)


{-| This accessor combinator lets you access the label at a particular TreePath.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    try (Tree.at [0]) simpleTree
    --> Just "a"

    map (Tree.at [0]) String.toUpper simpleTree
    --> tree "root" [ tree "A" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
at : TreePath -> Optic pr ls a a x y -> Traversal (Tree a) (Tree a) x y
at p =
    path p << label_


{-| This accessor combinator lets you access the label of a given Tree.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    get (Tree.label_) simpleTree
    --> "root"

    set (Tree.label_) "top 'o the mornin' to yah!" simpleTree
    --> tree "top 'o the mornin' to yah!" [ tree "a" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

    map (Tree.label_) String.toUpper simpleTree
    --> tree "ROOT" [ tree "a" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
label_ : Optic pr ls a a x y -> Lens ls (Tree a) (Tree a) x y
label_ =
    Base.lens "-label"
        Tree.label
        (\s b -> Tree.mapLabel (always b) s)


{-| This accessor combinator lets you access a sub-tree at a given TreePath.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    try (Tree.path [0,0]) simpleTree
    --> Just (leaf "b")

    set (Tree.path [1,0]) (tree "hey!" [leaf "deeper"]) simpleTree
    --> tree "root" [ tree "a" [ leaf "b" ] , tree "x" [ tree "hey!" [leaf "deeper"] ] ]

    map (Tree.path [0]) (Tree.mapLabel (\s -> "gimme an " ++ String.toUpper s ++ "!")) simpleTree
    --> tree "root" [ tree "gimme an A!" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
path : TreePath -> Optic pr ls (Tree a) (Tree a) x y -> Traversal (Tree a) (Tree a) x y
path p =
    Base.traversal
        ("<" ++ String.join ", " (List.map String.fromInt p) ++ ">")
        (Tree.at p >> Maybe.map List.singleton >> Maybe.withDefault [])
        (Tree.updateAt p)


{-| This accessor combinator lets you modify the type of each label of a Tree.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "r"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    all Tree.each simpleTree
    --> ["r", "a", "b", "x", "y"]

    set Tree.each "1" simpleTree
    --> tree "1" [ tree "1" [ leaf "1" ] , tree "1" [ leaf "1" ] ]

    map Tree.each (\s -> "gimme an " ++ String.toUpper s ++ "!") simpleTree
    --> tree "gimme an R!" [ tree "gimme an A!" [ leaf "gimme an B!" ] , tree "gimme an X!" [ leaf "gimme an Y!" ] ]

-}
each : Optic pr ls a b x y -> Traversal (Tree a) (Tree b) x y
each =
    Base.traversal "<>" Tree.flatten Tree.map
