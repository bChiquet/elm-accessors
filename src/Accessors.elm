module Accessors exposing
    ( Optic, SimpleOptic
    , Traversal, Lens, Prism, Iso
    , SimpleTraversal, SimpleLens, SimplePrism, SimpleIso
    , traversal, lens, prism, iso
    , ixT, ixL, ixP
    , get, all, try, has, map, set, new, name
    , just_, ok_, err_
    , values, keyed, key, keyI, key_
    , each, eachIdx, at
    , every, everyIdx, ix
    , fst, snd
    , Y
    )

{-| Accessors are a way of operating on nested data in Elm that doesn't require gobs of boilerplate.


## Optic: is the opaque underlying interface that enables the rest of the library to work.

@docs Optic, SimpleOptic


## Type Aliases: are shorthands from the Optics nomenclature that make writing your

own accessors more convenient and hopefully easier to understand.

@docs Traversal, Lens, Prism, Iso
@docs SimpleTraversal, SimpleLens, SimplePrism, SimpleIso


## Constructors

Accessors are built using these functions:

@docs traversal, lens, prism, iso


## Lifters for composing w/ indexed optics

@docs ixT, ixL, ixP


## Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, all, try, has, map, set, new, name


## Common Optics to mitigate `import` noise. Not everything is re-exported.

@docs just_, ok_, err_
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



-- Optic


type alias Optic pr ls s t a b =
    Base.Optic pr ls s t a b


type alias Y =
    Base.Y



-- Type Aliases


{-| The lens is "not a prism".
-}
type alias Lens ls s t a b =
    Base.Lens ls s t a b


{-| The prism is "not a lens".
-}
type alias Prism pr s t a b =
    Base.Prism pr s t a b


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal s t a b =
    Base.Traversal s t a b


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso pr ls s t a b =
    Base.Iso pr ls s t a b


{-| `Optic` that cannot change type of the object.
-}
type alias SimpleOptic pr ls s a =
    Optic pr ls s s a a


{-| `Lens` that cannot change type of the object.
-}
type alias SimpleLens ls s a =
    Lens ls s s a a


{-| `Prism` that cannot change type of the object.
-}
type alias SimplePrism pr s a =
    Prism pr s s a a


{-| `Traversal` that cannot change type of the object.
-}
type alias SimpleTraversal s a =
    Traversal s s a a


{-| `Iso` that cannot change type of the object.
-}
type alias SimpleIso pr ls s a =
    Iso pr ls s s a a



-- Constructors


{-| An isomorphism constructor.
-}
iso :
    String
    -> (s -> a)
    -> (b -> t)
    -> Base.Optic pr ls a b x y
    -> Base.Iso pr ls s t x y
iso =
    Base.iso


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Optic attr view over -> Optic { rec | foo : attr } view over
    foo =
        makeOneToOne
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
lens :
    String
    -> (s -> a)
    -> (s -> b -> t)
    -> Optic pr ls a b x y
    -> Base.Lens ls s t x y
lens =
    Base.lens


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out `a`.

-}
prism :
    String
    -> (b -> t)
    -> (s -> Result t a)
    -> Base.Optic pr ls a b x y
    -> Base.Prism pr s t x y
prism =
    Base.prism


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Optic pr ls a b x y -> Traversal (List a) (List b) x y
    each =
        Base.traversal "[]"
            identity
            List.map

-}
traversal :
    String
    -> (s -> List a)
    -> ((a -> b) -> s -> t)
    -> (Optic pr ls a b x y -> Traversal s t x y)
traversal =
    Base.traversal



-- Lifters for composing w/ indexed optics


ixT :
    (Optic pr ls x y x y -> Optic pr ls b t x y)
    -> Optic a c x y d e
    -> Traversal ( idx, b ) t d e
ixT =
    Base.ixT


ixL :
    (Optic pr Y x b x b -> Optic pr Y a t x b)
    -> Optic c ls x b d y
    -> Lens ls ( idx, a ) t d y
ixL =
    Base.ixL


ixP :
    (Optic Y ls value rte value rte -> Optic Y ls b t value rte)
    -> Optic pr a value rte x y
    -> Prism pr ( idx, b ) t x y
ixP =
    Base.ixP



-- Actions


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get : (Optic pr ls a b a b -> Optic pr Y s t a b) -> s -> a
get =
    Base.get


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just "Stuff"
        |> all just_
    --> ["Stuff"]

    Nothing
        |> all just_
    --> []

-}
all :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> List a
all =
    Base.all


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    ["Stuff", "things"]
        |> try (at 2)
    --> Nothing

    ["Stuff", "things"]
        |> try (at 0)
    --> Just "Stuff"

-}
try :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> Maybe a
try =
    Base.try


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> has just_
    --> True

    Nothing
        |> has just_
    --> False

    [ "Wooo", "Things" ]
        |> has (at 7)
    --> False

    [ "Wooo", "Things" ]
        |> has (at 0)
    --> True

-}
has : (Optic pr ls a b a b -> Optic pr ls s t a b) -> s -> Bool
has =
    Base.has


{-| The over function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
map (foo << qux) ((+) 1) myRecord
```

-}
map : (Optic pr ls a b a b -> Optic pr ls s t a b) -> (a -> b) -> s -> t
map =
    Base.map


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
set : (Optic pr ls a b a b -> Optic pr ls s t a b) -> b -> s -> t
set =
    Base.set


{-| Use prism to reconstruct.
-}
new : (Optic ps ls a b a b -> Optic Y ls s t a b) -> b -> t
new =
    Base.new


{-| This function gives the name of the function as a string...
-}
name : (Optic pr ls a b x y -> Optic pr ls s t x y) -> String
name =
    Base.name



-- Common Accessors


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
                  , qux = Nothing
                  }

    try (L.foo << just_ << L.bar << just_ << L.stuff) maybeRecord
    --> Just (Just 2 )

    try (L.qux << just_ << L.bar) maybeRecord
    --> Nothing

    map (L.foo << just_ << L.bar << just_ << L.stuff << just_) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}

    map (L.qux << just_ << L.bar << just_) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}

-}
just_ : Optic pr ls a b x y -> Prism pr (Maybe a) (Maybe b) x y
just_ =
    Maybe.just_



--{-| This accessor combinator lets you access values inside Maybe.
--see [`try`](Maybe-Accessors#try) for a NON-flattening lens.
--    import Accessors exposing (..)
--    import Lens as L
--    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
--    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
--                  , qux = Nothing
--                  }
--    try (L.foo << just__ << L.bar << just__ << L.stuff) maybeRecord
--    --> Just 2
--    try (L.qux << just__ << L.bar) maybeRecord
--    --> Nothing
--    map (L.foo << just__ << L.bar << just__ << L.stuff << just__) ((+) 1) maybeRecord
--    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}
--    map (L.qux << just__ << L.bar << just__) ((+) 1) maybeRecord
--    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}
---}
--try_ : Optic attr (Maybe view) over -> Optic (Maybe attr) (Maybe view) (Maybe over)
--try_ =
--    Maybe.try_
--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--    import Dict exposing (Dict)
--    import Lens as L
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    get (key "foo" << def {bar = 0}) dict
--    --> {bar = 2}
--    get (key "baz" << def {bar = 0}) dict
--    --> {bar = 0}
--    -- NOTE: The following do not compile :thinking:
--    --get (key "foo" << just_ << L.bar << def 0) dict
--    ----> 2
--    --get (key "baz" << just_ << L.bar << def 0) dict
--    ----> 0
---}
--def : attr -> Optic attr view over -> Optic (Maybe attr) view (Maybe over)
--def =
--    Maybe.def
--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--    import Dict exposing (Dict)
--    import Lens as L
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    -- NOTE: Use `def` for this.
--    --get (key "foo" << or {bar = 0}) dict
--    ----> {bar = 2}
--    --get (key "baz" << or {bar = 0}) dict
--    ----> {bar = 0}
--    get ((key "foo" << just_ << L.bar) |> or 0) dict
--    --> 2
--    get ((key "baz" << just_ << L.bar) |> or 0) dict
--    --> 0
---}
--or : attr -> (Optic attr attr attrOver -> Optic value (Maybe attr) over) -> Optic attr attrView attrOver -> Optic value attrView over
--or =
--    Maybe.or


{-| This accessor combinator lets you access values inside List.
alias for [`List.Accessors.each`](List-Accessors#each)
-}
each : Optic pr ls a b x y -> Traversal (List a) (List b) x y
each =
    List.each


{-| This accessor lets you traverse a list including the index of each element
alias for [`List.Accessors.each_`](List-Accessors#each_)
-}
eachIdx : Optic pr ls ( Int, b ) c x y -> Traversal (List b) (List c) x y
eachIdx =
    List.each_


{-| at: Structure Preserving accessor over List members.
alias for [`List.Accessors.at`](List-Accessors#at)
-}
at : Int -> Optic pr ls a a x y -> Traversal (List a) (List a) x y
at =
    List.at


{-| This accessor combinator lets you access values inside Array.
alias for [`Array.Accessors.each`](Array-Accessors#each)

    import Accessors exposing (..)
    import Array exposing (Array)
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    all (L.foo << every << L.bar) arrayRecord
    --> [2, 3, 4]

    map (L.foo << every << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Optic pr ls a b x y -> Traversal (Array a) (Array b) x y
every =
    Array.each


{-| This accessor lets you traverse an Array including the index of each element
alias for [`Array.Accessors.each_`](Array-Accessors#each_)

    import Accessors exposing (..)
    import Lens as L
    import Array exposing (Array)

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord = { foo = [ {bar = 2}
                          , {bar = 3}
                          , {bar = 4}
                          ] |> Array.fromList
                  }

    multiplyIfGTOne : (Int, { bar : Int }) -> { bar : Int }
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            { bar = bar * 10 }
        else
            rec


    all (L.foo << everyIdx) arrayRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})]

    map (L.foo << everyIdx) multiplyIfGTOne arrayRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}

    all (L.foo << everyIdx << ixL L.bar) arrayRecord
    --> [2, 3, 4]

    map (L.foo << everyIdx << ixL L.bar) ((+) 1) arrayRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}

-}
everyIdx : Optic pr ls ( Int, b ) c x y -> Traversal (Array b) (Array c) x y
everyIdx =
    Array.each_


{-| alias for [`Array.Accessors.at`](Array-Accessors#at)

    import Accessors exposing (..)
    import Array exposing (Array)
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    try (ix 1) arr
    --> Just { bar = "Things" }

    try (ix 9000) arr
    --> Nothing

    try (ix 0 << L.bar) arr
    --> Just "Stuff"

    set (ix 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << L.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Optic pr ls a a x y -> Traversal (Array a) (Array a) x y
ix =
    Array.at


{-| This accessor lets you access values inside the Ok variant of a Result.
alias for [`Result.Accessors.onOk`](Result-Accessors#onOk)

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << ok_ << L.bar) maybeRecord
    --> Just 2

    try (L.qux << ok_ << L.bar) maybeRecord
    --> Nothing

    map (L.foo << ok_ << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    map (L.qux << ok_ << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok_ : Optic pr ls a b x y -> Prism pr (Result ignored a) (Result ignored b) x y
ok_ =
    Result.ok_


{-| This accessor lets you access values inside the Err variant of a Result.
alias for [`Result.Accessors.onErr`](Result-Accessors#onErr)

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << err_) maybeRecord
    --> Nothing

    try (L.qux << err_) maybeRecord
    --> Just "Not an Int"

    map (L.foo << err_) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    map (L.qux << err_) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
err_ : Optic pr ls a b x y -> Prism pr (Result a ignored) (Result b ignored) x y
err_ =
    Result.err_


{-| values: This accessor lets you traverse a Dict including the index of each element
alias for [`Dict.Accessors.each`](Dict-Accessors#each)

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    all (L.foo << values) dictRecord
    --> [{bar = 2}, {bar = 3}, {bar = 4}]

    map (L.foo << values << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    all (L.foo << values << L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << values << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
values : Optic pr ls a b x y -> Traversal (Dict key a) (Dict key b) x y
values =
    Dict.each


{-| keyed: This accessor lets you traverse a Dict including the index of each element
alias for [`Dict.Accessors.each_`](Dict-Accessors#each_)

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    multiplyIfA : (String, { bar : Int }) -> { bar : Int }
    multiplyIfA ( key, ({ bar } as rec) ) =
        if key == "a" then
            { bar = bar * 10 }
        else
            rec


    all (L.foo << keyed) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})]

    map (L.foo << keyed) multiplyIfA dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList}

    all (L.foo << keyed << ixL L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << keyed << ixL L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
keyed : Optic pr ls ( a, b ) c x y -> Traversal (Dict a b) (Dict a c) x y
keyed =
    Dict.each_


{-| key: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.at`](Dict-Accessors#at)

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

    try (key "foo" << just_ << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << just_ << L.bar) 3 dict
    --> dict

-}
key : String -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict String a) (Dict String a) x y
key =
    Dict.at


{-| key: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.id`](Dict-Accessors#id)

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

    try (keyI 1 << just_ << L.bar) dict
    --> Just 2

    set (keyI 1) Nothing dict
    --> Dict.remove 1 dict

    set (keyI 0 << just_ << L.bar) 3 dict
    --> dict

-}
keyI : Int -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict Int a) (Dict Int a) x y
keyI =
    Dict.id


{-| `key_`: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.at_`](Dict-Accessors#at_)

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict Char {bar : Int}
    dict = Dict.fromList [('C', {bar = 2})]

    keyC : Char -> Optic pr ls (Maybe {bar : Int}) (Maybe {bar : Int}) x y -> Lens ls (Dict Char {bar : Int}) (Dict Char {bar : Int}) x y
    keyC =
        key_ String.fromChar

    get (keyC 'C') dict
    --> Just {bar = 2}

    get (keyC 'Z') dict
    --> Nothing

    try (keyC 'C' << just_ << L.bar) dict
    --> Just 2

    set (keyC 'C') Nothing dict
    --> Dict.remove 'C' dict

    set (keyC 'Z' << just_ << L.bar) 3 dict
    --> dict

-}
key_ : (comparable -> String) -> comparable -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict comparable a) (Dict comparable a) x y
key_ =
    Dict.at_


{-| Lens over the first component of a Tuple
alias for [`Tuple.Accessors.fst`](Tuple-Accessors#fst)

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get fst charging
    --> "It's over"

    set fst "It's over" charging
    --> ("It's over", 1)

    map fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Optic pr ls a b x y -> Lens ls ( a, two ) ( b, two ) x y
fst =
    Tuple.fst


{-| alias for [`Tuple.Accessors.snd`](Tuple-Accessors#snd)

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get snd meh
    --> 1

    set snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set snd 1125
        |> map fst (\s -> String.toUpper s ++ "!!!")
        |> map snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Optic pr ls a b x y -> Lens ls ( one, a ) ( one, b ) x y
snd =
    Tuple.snd
