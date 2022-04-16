module Accessors exposing
    ( Relation, Property
    , get, set, over, name
    , try
    , key
    , each, at
    , every, ix
    , one, two
    , makeOneToOne, makeOneToN
    --, def
    )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.


# Relation

@docs Relation, Property


# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name


# Common accessors

@docs try, def
@docs key
@docs each, at
@docs every, ix
@docs one, two


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

-}

import Array exposing (Array)
import Dict exposing (Dict)


type alias Property s a wrap =
    Relation a a a -> Relation s a wrap


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Relation super sub wrap
    = Relation
        { get : super -> wrap
        , over : (sub -> sub) -> (super -> super)
        , name : String
        }


{-| id is a neutral `Relation`. It is used to end a braid of accessors (see
the implementation for get, set and over).
-}
id : Relation a a a
id =
    Relation
        { get = \a -> a
        , over = \change -> \a -> change a
        , name = ""
        }


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get : Property super sub wrap -> super -> wrap
get accessor s =
    let
        (Relation relation) =
            accessor id
    in
    relation.get s


{-| This function gives the name of the function as a string...
-}
name : Property super sub wrap -> String
name accessor =
    let
        (Relation relation) =
            accessor id
    in
    relation.name


{-| The set function takes:

  - An accessor,
  - A value of the type `sub`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed to be
    the set value.

```
set (foo << bar) "Hi!" myRecord
```

-}
set : Property super sub wrap -> sub -> super -> super
set accessor value s =
    let
        (Relation relation) =
            accessor id

        newSuper =
            relation.over (\_ -> value) s
    in
    if get accessor newSuper /= get accessor s then
        newSuper

    else
        s


{-| The over function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
over (foo << qux) ((+) 1) myRecord
```

-}
over : Property super sub wrap -> (sub -> sub) -> super -> super
over accessor change s =
    let
        (Relation relation) =
            accessor id

        newSuper =
            relation.over change s
    in
    if get accessor newSuper /= get accessor s then
        newSuper

    else
        s


{-| This function lets you build an accessor for containers that have
a 1:1 relation with what they contain, such as a record and one of its fields:

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    String
    -> (super -> sub)
    -> ((sub -> sub) -> super -> super)
    -> Relation sub reachable wrap
    -> Relation super reachable wrap
makeOneToOne n getter mapper (Relation sub) =
    Relation
        { get = \super -> sub.get (getter super)
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:

    onEach : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    onEach =
        makeOneToN
            List.map
            List.map

n.b. implementing those is usually considerably simpler than the type suggests.

-}
makeOneToN :
    String
    -> ((sub -> subWrap) -> super -> superWrap)
    -> ((sub -> sub) -> super -> super)
    -> Relation sub reachable subWrap
    -> Relation super reachable superWrap
makeOneToN n getter mapper (Relation sub) =
    Relation
        { get = \super -> getter sub.get super
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import Test.Accessors.Record as R

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (R.foo << each << R.bar) listRecord
    --> [2, 3, 4]

    over (R.foo << each << R.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Relation super sub wrap -> Relation (List super) sub (List wrap)
each =
    makeOneToN ":[]" List.map List.map


{-| This accessor combinator lets you access values inside Array.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Test.Accessors.Record as R

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (R.foo << every << R.bar) arrayRecord
    --> Array.fromList [2, 3, 4]

    over (R.foo << every << R.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Relation super sub wrap -> Relation (Array super) sub (Array wrap)
every =
    makeOneToN "[]" Array.map Array.map


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Test.Accessors.Record as R

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    get (R.foo << try << R.bar) maybeRecord
    --> Just 2

    get (R.qux << try << R.bar) maybeRecord
    --> Nothing

    over (R.foo << try << R.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (R.qux << try << R.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation sub path wrap -> Relation (Maybe sub) path (Maybe wrap)
try =
    makeOneToN "?" Maybe.map Maybe.map



--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--  TODO: Doesn't do what is expected... :/
--    import Dict exposing (Dict)
--    import Test.Accessors.Record as R
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    get (key "foo" << def {bar = 0}) dict
--    --> {bar = 2}
--    get (key "baz" << def {bar = 0}) dict
--    --> {bar = 0}
--    get (key "foo" << try << R.bar << def 0) dict
--    --> 2
--    get (key "baz" << try << R.bar << def 0) dict
--    --> 0
---}
--def : sub -> Relation sub reachable sub -> Relation (Maybe sub) reachable sub
--def d =
--    makeOneToN "??"
--        (\f -> over try f >> Maybe.withDefault d)
--        Maybe.map


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Test.Accessors.Record as R

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo") dict
    --> Just {bar = 2}

    get (key "baz") dict
    --> Nothing

    get (key "foo" << try << R.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << try << R.bar) 3 dict
    --> dict

-}
key : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
key k =
    makeOneToOne "{}" (Dict.get k) (Dict.update k)


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (..)
    import Test.Accessors.Record as R

    list : List { bar : String }
    list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (at 1) list
    --> Just { bar = "Things" }

    get (at 9000) list
    --> Nothing

    get (at 0 << R.bar) list
    --> Just "Stuff"

    set (at 0 << R.bar) "Whatever" list
    --> [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (at 9000 << R.bar) "Whatever" list
    --> list

-}
at : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
at idx =
    makeOneToOne ("(" ++ String.fromInt idx ++ ")")
        (if idx < 0 then
            always Nothing

         else
            List.head << List.drop idx
        )
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: write this in terms of `foldr` to avoid double iteration.
            List.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> List.filterMap identity
        )
        << try


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Test.Accessors.Record as R

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (ix 1) arr
    --> Just { bar = "Things" }

    get (ix 9000) arr
    --> Nothing

    get (ix 0 << R.bar) arr
    --> Just "Stuff"

    set (ix 0 << R.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << R.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
ix idx =
    makeOneToOne
        ("[" ++ String.fromInt idx ++ "]")
        (Array.get idx)
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: there's a better way to write this no doubt.
            Array.indexedMap
                (\idx_ v ->
                    if idx == idx_ then
                        fn (Just v)

                    else
                        Just v
                )
                >> Array.foldl
                    (\e acc ->
                        case e of
                            Just v ->
                                Array.push v acc

                            Nothing ->
                                acc
                    )
                    Array.empty
        )
        << try


{-| Lens over the first component of a Tuple

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get one charging
    --> "It's over"

    set one "It's over" charging
    --> ("It's over", 1)

    over one (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
one : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
one =
    makeOneToOne "_1" Tuple.first Tuple.mapFirst


{-|

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get two meh
    --> 1

    set two 1125 meh
    --> ("It's over", 1125)

    meh
        |> set two 1125
        |> over one (\s -> String.toUpper s ++ "!!!")
        |> over two ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
two : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
two =
    makeOneToOne "_2" Tuple.second Tuple.mapSecond
