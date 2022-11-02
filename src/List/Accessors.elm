module List.Accessors exposing (each, each_, at, id)

{-| List.Accessors

@docs each, each_, at, id

-}

import Base exposing (Optic(..), Traversal)
import Tuple.Accessors as Tuple



-- import Maybe.Accessors as Maybe


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import List.Accessors as List
    import Lens as L

    listRecord : { foo : List { bar : Int } }
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    all (L.foo << List.each << L.bar) listRecord
    --> [2, 3, 4]

    map (L.foo << List.each << L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Optic pr ls a b x y -> Traversal (List a) (List b) x y
each =
    Base.traversal ":[]" identity List.map


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import List.Accessors as List
    import Lens as L

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    multiplyIfGTOne : (Int, { bar : Int }) -> { bar : Int }
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
             { bar = bar * 10 }
        else
            rec

    all (L.foo << List.each_) listRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})]

    map (L.foo << List.each_) multiplyIfGTOne listRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}]}

    all (L.foo << List.each_ << ixL L.bar) listRecord
    --> [2, 3, 4]

    map (L.foo << List.each_ << ixL L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each_ : Optic pr ls ( Int, b ) c x y -> Traversal (List b) (List c) x y
each_ =
    Base.traversal "#[]"
        (List.indexedMap Tuple.pair)
        (\fn -> List.indexedMap (\idx -> Tuple.pair idx >> fn))


{-| at: Structure Preserving accessor over List members.

    import Accessors exposing (..)
    import List.Accessors as List
    import Lens as L

    list : List { bar : String }
    list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    try (List.at 1) list
    --> Just { bar = "Things" }

    try (List.at 9000) list
    --> Nothing

    try (List.at 0 << L.bar) list
    --> Just "Stuff"

    set (List.at 0 << L.bar) "Whatever" list
    --> [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (List.at 9000 << L.bar) "Whatever" list
    --> list

-}
at : Int -> Optic pr ls a a x y -> Traversal (List a) (List a) x y
at key =
    Base.traversal ("[" ++ String.fromInt key ++ "]?")
        (List.foldl
            (\v ( idx, acc ) ->
                ( idx + 1
                , if idx == key then
                    v :: acc

                  else
                    acc
                )
            )
            ( 0, [] )
            >> Tuple.second
        )
        (\fn ->
            List.indexedMap
                (\idx v ->
                    if idx == key then
                        fn v

                    else
                        v
                )
        )



-- TODO: Add these back in the future maybe?
-- {-| Everything except first n elements of the list.
-- -}
-- drop : Int -> Optic pr ls (List a) (List a) x y -> Traversal (List a) (List a) x y
-- drop i =
--     Base.traversal "drop" (List.drop i >> (\x -> [ x ])) <|
--         \f lst ->
--             List.take i lst ++ f (List.drop i lst)
-- {-| List head.
-- -}
-- head : Optic pr ls a a x y -> Traversal (List a) (List a) x y
-- head =
--     cons << Tuple.fst
-- {-| Match head/tail of a non-empty list.
-- -}
-- cons : Optic pr ls ( a, List a ) ( b, List b ) x y -> Prism pr (List a) (List b) x y
-- cons =
--     Base.prism ":" (uncurry (::)) <|
--         \lst ->
--             case lst of
--                 x :: xs ->
--                     Ok ( x, xs )
--                 [] ->
--                     Err []


{-| id: Structure Preserving accessor over List members.

    import Accessors exposing (..)
    import List.Accessors as List
    import Lens as L

    list : List { id : Int, bar : String }
    list = [{ id = 7, bar = "Stuff" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]

    try (List.id 1) list
    --> Just { id = 1, bar = "Things" }

    try (List.id 0) list
    --> Nothing

    try (List.id 7 << L.bar) list
    --> Just "Stuff"

    set (List.id 7 << L.bar) "Whatever" list
    --> [{ id = 7, bar = "Whatever" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]

    set (List.id 9000 << L.bar) "Whatever" list
    --> list

-}
id : Int -> Optic pr ls { a | id : Int } { a | id : Int } x y -> Traversal (List { a | id : Int }) (List { a | id : Int }) x y
id key =
    Base.traversal ("(" ++ String.fromInt key ++ ")?")
        (List.filterMap
            (\v ->
                if v.id == key then
                    Just v

                else
                    Nothing
            )
        )
        (\fn ->
            List.map
                (\v ->
                    if v.id == key then
                        fn v

                    else
                        v
                )
        )
