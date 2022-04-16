module Accessors exposing
    ( Relation, Property
    , get, set, over, name
    , try, def
    , key
    , each, at
    , every, ix
    , one, two
    , makeOneToOne, makeOneToN
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

    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (foo << each << bar) listRecord
    -- returns [2, 3, 4]

    over (foo << each << bar) ((+) 1) listRecord
    -- returns {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Relation super sub wrap -> Relation (List super) sub (List wrap)
each =
    makeOneToN ":[]" List.map List.map


{-| This accessor combinator lets you access values inside Array.

    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (foo << each << bar) arrayRecord
    -- returns [2, 3, 4]

    over (foo << each << bar) ((+) 1) arrayRecord
    -- returns {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Relation super sub wrap -> Relation (Array super) sub (Array wrap)
every =
    makeOneToN "[]" Array.map Array.map


{-| This accessor combinator lets you access values inside Maybe.

    maybeRecord = { foo = Just {bar = 2}
                  , qux = Nothing
                  }

    get (foo << try << bar) maybeRecord
    -- returns Just 2

    get (qux << try << bar) maybeRecord
    -- returns Nothing

    over (foo << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 3}, qux = Nothing}

    over (qux << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation sub path wrap -> Relation (Maybe sub) path (Maybe wrap)
try =
    makeOneToN "?" Maybe.map Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo" << def {bar = 0}) dict
    -- returns {bar = 2}

    get (key "baz" << def {bar = 0}) dict
    -- returns {bar = 0}

    get (key "foo" << try << bar << def 0) dict
    -- returns 2

    get (key "baz" << try << bar << def 0) dict
    -- returns 0

-}
def : b -> Relation b reachable wrap -> Relation (Maybe b) reachable wrap
def d =
    makeOneToOne "??" (Maybe.withDefault d) Maybe.map


{-| This accessor combinator lets you access Dict members.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    dict = Dict.fromList [("foo", {bar = 2})]

    get (dictEntry "foo") dict
    -- returns Just {bar = 2}

    get (dictEntry "baz" dict)
    -- returns Nothing

    get (dictEntry "foo" << try << bar) dict
    -- returns Just 2

    set (dictEntry "foo") Nothing dict
    -- returns Dict.remove "foo" dict

    set (dictEntry "baz" << try << bar) 3 dict
    -- returns dict

-}
key : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
key k =
    makeOneToOne "{}" (Dict.get k) (Dict.update k)


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


one : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
one =
    makeOneToOne "_1" Tuple.first Tuple.mapFirst


two : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
two =
    makeOneToOne "_2" Tuple.second Tuple.mapSecond
