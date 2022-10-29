module Accessors exposing
    ( Relation, Accessor, Lens, Lens_, Setable
    , makeOneToOne, makeOneToN
    , makeOneToOne_, makeOneToN_
    , get, set, over, name, is
    , try, def, or, ok, err
    , values, keyed, key, keyI, key_
    , each, eachIdx, at
    , every, everyIdx, ix
    , fst, snd
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

@docs Relation, Accessor, Lens, Lens_, Setable


# Constructors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN
@docs makeOneToOne_, makeOneToN_


# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name, is


# Common accessors

@docs try, def, or, ok, err
@docs values, keyed, key, keyI, keyF, key_
@docs each, eachIdx, at
@docs every, everyIdx, ix
@docs fst, snd

-}

import Array exposing (Array)
import Array.Accessors as Array
import Base
import Dict exposing (Dict)
import Dict.Accessors as Dict
import List.Accessors as List
import Maybe.Accessors as Maybe
import Result.Accessors as Result
import Tuple.Accessors as Tuple



-- Relation


{-| The most general version of this type that everything else specializes
-}
type alias Accessor dataBefore dataAfter attrBefore attrAfter reachable =
    Base.Accessor dataBefore dataAfter attrBefore attrAfter reachable


{-| This is an approximation of Van Laarhoven encoded Lenses which enable the
the callers to use regular function composition to build more complex nested
updates of more complicated types.

But the original "Lens" type looked more like:

    type alias Lens structure attribute =
        { get : structure -> attribute
        , set : structure -> attribute -> structure
        }

unfortunately these can't be composed without
defining custom `composeLens`, `composeIso`, `composePrism`, style functions.

whereas with this approach we're able to make use of Elm's built in `<<` operator
to get/set/over deeply nested data.

-}
type alias
    Lens
        -- Structure Before Action
        structure
        -- Structure After Action
        transformed
        -- Focus Before action
        attribute
        -- Focus After action
        built
    =
    Base.Lens structure transformed attribute built


{-| Simplified version of Lens but seems to break type inference for more complicated compositions.
-}
type alias Lens_ structure attribute =
    Lens structure attribute attribute attribute



-- type alias Getable structure transformed attribute built reachable =
--     Relation attribute built attribute
--     -> Relation structure reachable transformed


{-| Type of a composition of accessors that `set` can be called with.
-}
type alias Setable structure transformed attribute built =
    Relation attribute attribute built -> Relation structure attribute transformed



-- type alias Watami structure transformed attribute built =
--     Relation attribute (Maybe built) transformed
--     -> Relation structure (Maybe built) (Maybe transformed)


type alias Relation structure attribute wrap =
    Base.Relation structure attribute wrap



-- Constructors


{-| This function lets you build an accessor for containers that have
a 1:1 relation with what they contain, such as a record and one of its fields:

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> (Relation attribute reachable wrap -> Relation structure reachable wrap)
makeOneToOne =
    Base.makeOneToOne


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne_
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne_ :
    String
    -> (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> (Relation attribute reachable wrap -> Relation structure reachable wrap)
makeOneToOne_ =
    Base.makeOneToOne_


{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN
            List.map
            List.map

n.b. implementing those is usually considerably simpler than the type suggests.

-}
makeOneToN :
    ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here? And this is obviously not Lens so?
    -> (Relation attribute reachable built -> Relation structure reachable transformed)
makeOneToN =
    Base.makeOneToN


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN_ "[]"
            List.map
            List.map

-}
makeOneToN_ :
    String
    -> ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here?
    -> Relation attribute reachable built
    -> Relation structure reachable transformed
makeOneToN_ =
    Base.makeOneToN_



-- Actions


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get :
    (Relation attribute built attribute -> Relation structure reachable transformed)
    -> structure
    -> transformed
get =
    Base.get


{-| This function gives the name of the function as a string...
-}
name : Accessor a b c d e -> String
name =
    Base.name


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
set :
    Setable structure transformed attribute built
    -> attribute
    -> structure
    -> structure
set =
    Base.set



-- type alias Modifiable =
--    Relation attribute x y -> Relation structure a transformed


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
over :
    (Relation attribute attribute built -> Relation structure attribute transformed)
    -> (attribute -> attribute)
    -> structure
    -> structure
over =
    Base.over



-- Common Accessors


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    get (L.foo << try << L.bar) maybeRecord
    --> Just 2

    get (L.qux << try << L.bar) maybeRecord
    --> Nothing

    over (L.foo << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (L.qux << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
try =
    Maybe.try


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    get (key "foo" << def {bar = 0}) dict
    --> {bar = 2}

    get (key "baz" << def {bar = 0}) dict
    --> {bar = 0}

    -- NOTE: The following do not compile :thinking:
    --get (key "foo" << try << L.bar << def 0) dict
    ----> 2

    --get (key "baz" << try << L.bar << def 0) dict
    ----> 0

-}
def : attribute -> Relation attribute reachable wrap -> Relation (Maybe attribute) reachable wrap
def =
    Maybe.def


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    -- NOTE: Use `def` for this.
    --get (key "foo" << or {bar = 0}) dict
    ----> {bar = 2}

    --get (key "baz" << or {bar = 0}) dict
    ----> {bar = 0}

    get ((key "foo" << try << L.bar) |> or 0) dict
    --> 2

    get ((key "baz" << try << L.bar) |> or 0) dict
    --> 0

-}
or :
    attribute
    -> (Relation attribute attribute attribute -> Relation structure attribute (Maybe attribute))
    -> (Relation attribute other attribute -> Relation structure other attribute)
or =
    Maybe.or


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import Lens as L

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (L.foo << each << L.bar) listRecord
    --> [2, 3, 4]

    over (L.foo << each << L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
each =
    List.each


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import Lens as L

    listRecord : {foo : List {bar : Int}}
    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)


    get (L.foo << eachIdx) listRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})]

    over (L.foo << eachIdx) multiplyIfGTOne listRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}]}

    get (L.foo << eachIdx << snd << L.bar) listRecord
    --> [2, 3, 4]

    over (L.foo << eachIdx << snd << L.bar) ((+) 1) listRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
eachIdx : Relation ( Int, attribute ) reachable built -> Relation (List attribute) reachable (List built)
eachIdx =
    List.each_


{-| at: Structure Preserving accessor over List members.

    import Accessors exposing (..)
    import Lens as L

    list : List { bar : String }
    list = [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (at 1) list
    --> Just { bar = "Things" }

    get (at 9000) list
    --> Nothing

    get (at 0 << L.bar) list
    --> Just "Stuff"

    set (at 0 << L.bar) "Whatever" list
    --> [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (at 9000 << L.bar) "Whatever" list
    --> list

-}
at : Int -> Relation v reachable wrap -> Relation (List v) reachable (Maybe wrap)
at =
    List.at


{-| This accessor combinator lets you access values inside Array.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (L.foo << every << L.bar) arrayRecord
    --> Array.fromList [2, 3, 4]

    over (L.foo << every << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Relation attribute built transformed -> Relation (Array attribute) built (Array transformed)
every =
    Array.each


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import Lens as L
    import Array exposing (Array)

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord = { foo = [ {bar = 2}
                          , {bar = 3}
                          , {bar = 4}
                          ] |> Array.fromList
                  }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)


    get (L.foo << everyIdx) arrayRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})] |> Array.fromList

    over (L.foo << everyIdx) multiplyIfGTOne arrayRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}

    get (L.foo << everyIdx << snd << L.bar) arrayRecord
    --> [2, 3, 4] |> Array.fromList

    over (L.foo << everyIdx << snd << L.bar) ((+) 1) arrayRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}

-}
everyIdx : Relation ( Int, attribute ) reachable built -> Relation (Array attribute) reachable (Array built)
everyIdx =
    Array.each_


{-| This accessor lets you access values inside the Ok variant of a Result.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    get (L.foo << ok << L.bar) maybeRecord
    --> Just 2

    get (L.qux << ok << L.bar) maybeRecord
    --> Nothing

    over (L.foo << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    over (L.qux << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok : Relation attribute built transformed -> Relation (Result x attribute) built (Maybe transformed)
ok =
    Result.onOk


{-| This accessor lets you access values inside the Err variant of a Result.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    get (L.foo << err) maybeRecord
    --> Nothing

    get (L.qux << err) maybeRecord
    --> Just "Not an Int"

    over (L.foo << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    over (L.qux << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
err : Relation attribute built transformed -> Relation (Result attribute x) built (Maybe transformed)
err =
    Result.onErr


{-| values: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    get (L.foo << values) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList

    over (L.foo << values << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    get (L.foo << values << L.bar) dictRecord
    --> [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList

    over (L.foo << values << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
values : Relation attribute reachable built -> Relation (Dict comparable attribute) reachable (Dict comparable built)
values =
    Dict.each


{-| keyed: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    multiplyIfA : (String, { bar : Int }) -> (String, { bar : Int })
    multiplyIfA ( key, ({ bar } as rec) ) =
        if key == "a" then
            ( key, { bar = bar * 10 } )
        else
            (key, rec)


    get (L.foo << keyed) dictRecord
    --> [("a", ("a", {bar = 2})), ("b", ("b", {bar = 3})), ("c", ("c", {bar = 4}))] |> Dict.fromList

    over (L.foo << keyed) multiplyIfA dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList}

    get (L.foo << keyed << snd << L.bar) dictRecord
    --> [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList

    over (L.foo << keyed << snd << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
keyed : Relation ( comparable, attribute ) reachable built -> Relation (Dict comparable attribute) reachable (Dict comparable built)
keyed =
    Dict.each_


{-| key: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo") dict
    --> Just {bar = 2}

    get (key "baz") dict
    --> Nothing

    get (key "foo" << try << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << try << L.bar) 3 dict
    --> dict

-}
key : String -> Relation (Maybe attribute) reachable wrap -> Relation (Dict String attribute) reachable wrap
key =
    key_ identity


{-| key: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict Int {bar : Int}
    dict = Dict.fromList [(1, {bar = 2})]

    get (keyI 1) dict
    --> Just {bar = 2}

    get (keyI 0) dict
    --> Nothing

    get (keyI 1 << try << L.bar) dict
    --> Just 2

    set (keyI 1) Nothing dict
    --> Dict.remove 1 dict

    set (keyI 0 << try << L.bar) 3 dict
    --> dict

-}
keyI : Int -> Relation (Maybe attribute) reachable wrap -> Relation (Dict Int attribute) reachable wrap
keyI =
    Dict.id


{-| `key_`: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict Char {bar : Int}
    dict = Dict.fromList [('C', {bar = 2})]

    keyC : Char -> Relation (Maybe attribute) reachable wrap -> Relation (Dict Char attribute) reachable wrap
    keyC =
        key_ String.fromChar

    get (keyC 'C') dict
    --> Just {bar = 2}

    get (keyC 'Z') dict
    --> Nothing

    get (keyC 'C' << try << L.bar) dict
    --> Just 2

    set (keyC 'C') Nothing dict
    --> Dict.remove 'C' dict

    set (keyC 'Z' << try << L.bar) 3 dict
    --> dict

-}
key_ : (comparable -> String) -> comparable -> Relation (Maybe attribute) reachable wrap -> Relation (Dict comparable attribute) reachable wrap
key_ =
    Dict.at_


{-| This accessor combinator lets you access Array indices.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Array exposing (Array)
    import Accessors exposing (..)
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (ix 1) arr
    --> Just { bar = "Things" }

    get (ix 9000) arr
    --> Nothing

    get (ix 0 << L.bar) arr
    --> Just "Stuff"

    set (ix 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << L.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
ix =
    Array.at


{-| Lens over the first component of a Tuple

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get fst charging
    --> "It's over"

    set fst "It's over" charging
    --> ("It's over", 1)

    over fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Relation sub reachable wrap -> Relation ( sub, x ) reachable wrap
fst =
    Tuple.fst


{-|

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get snd meh
    --> 1

    set snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set snd 1125
        |> over fst (\s -> String.toUpper s ++ "!!!")
        |> over snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Relation sub reachable wrap -> Relation ( x, sub ) reachable wrap
snd =
    Tuple.snd


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> is try
    --> True

    Nothing
        |> is try
    --> False

    ["Stuff", "things"]
        |> is (at 2)
    --> False

    ["Stuff", "things"]
        |> is (at 0)
    --> True

-}
is :
    (Relation attribute built attribute -> Relation structure reachable (Maybe transformed))
    -> structure
    -> Bool
is =
    Base.is
