module Accessors exposing
    ( Optic
    , makeOneToOne, makeOneToN
    , get, set, over, name, is
    , try, def, or, ok, err
    , values, keyed, key, keyI, key_
    , each, eachIdx, at
    , every, everyIdx, ix
    , fst, snd
    )

{-| Accessors are a way of operating on nested data in Elm that doesn't require gobs of boilerplate.


## Optic: is the opaque underlying interface that enables the rest of the library to work.

@docs Optic


## Type Aliases: are shorthands from the Optics nomenclature that make writing your

own accessors more convenient and hopefully easier to understand.

@docs Optic, Lens


## Constructors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN


## Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name, is


## Common Optics to mitigate `import` noise. Not everything is re-exported.

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



-- Optic


type alias Optic value view over =
    Base.Optic value view over



-- Type Aliases
-- Constructors


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
makeOneToOne :
    String
    -> (value -> attr)
    -> ((attr -> attrOver) -> value -> over)
    -> Optic attr attrView attrOver
    -> Optic value attrView over
makeOneToOne =
    Base.makeOneToOne


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Optic attr view over -> Optic (List attr) view (List over)
    each =
        makeOneToN "[]"
            List.map
            List.map

-}
makeOneToN :
    String
    -> ((attr -> attrView) -> (value -> view))
    -> ((attr -> attrOver) -> (value -> over))
    -> Optic attr attrView attrOver
    -> Optic value view over
makeOneToN =
    Base.makeOneToN



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
    (Optic attr attr attrOver -> Optic value view over)
    -> value
    -> view
get =
    Base.get


{-| This function gives the name of the function as a string...
-}
name : (Optic attr attrView attrOver -> Optic value view over) -> String
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
set : (Optic attr attrView attrOver -> Optic value view over) -> attrOver -> value -> over
set =
    Base.set



-- type alias Modifiable =
--    Optic attr attrView attrOver -> Optic value view over


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
over : (Optic attr attrView attrOver -> Optic value view over) -> (attr -> attrOver) -> value -> over
over =
    Base.over


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
is : (Optic attr attr attrOver -> Optic value (Maybe view) over) -> value -> Bool
is =
    Base.is



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
try : Optic attr view over -> Optic (Maybe attr) (Maybe view) (Maybe over)
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
def : attr -> Optic attr view over -> Optic (Maybe attr) view (Maybe over)
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
or : attr -> (Optic attr attr attrOver -> Optic value (Maybe attr) over) -> Optic attr attrView attrOver -> Optic value attrView over
or =
    Maybe.or


{-| This accessor combinator lets you access values inside List.
alias for [`List.Accessors.each`](List-Accessors#each)
-}
each : Optic attr view over -> Optic (List attr) (List view) (List over)
each =
    List.each


{-| This accessor lets you traverse a list including the index of each element
alias for [`List.Accessors.each_`](List-Accessors#each_)
-}
eachIdx : Optic ( Int, attr ) view ( ignored, over ) -> Optic (List attr) (List view) (List over)
eachIdx =
    List.each_


{-| at: Structure Preserving accessor over List members.
alias for [`List.Accessors.at`](List-Accessors#at)
-}
at : Int -> Optic attr view attr -> Optic (List attr) (Maybe view) (List attr)
at =
    List.at


{-| This accessor combinator lets you access values inside Array.
alias for [`Array.Accessors.each`](Array-Accessors#each)

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
every : Optic attr view over -> Optic (Array attr) (Array view) (Array over)
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
everyIdx : Optic ( Int, attr ) view ( ignored, over ) -> Optic (Array attr) (Array view) (Array over)
everyIdx =
    Array.each_


{-| alias for [`Array.Accessors.at`](Array-Accessors#at)

    import Accessors exposing (..)
    import Array exposing (Array)
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
ix : Int -> Optic over view over -> Optic (Array over) (Maybe view) (Array over)
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

    get (L.foo << ok << L.bar) maybeRecord
    --> Just 2

    get (L.qux << ok << L.bar) maybeRecord
    --> Nothing

    over (L.foo << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    over (L.qux << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok : Optic attr view over -> Optic (Result ignored attr) (Maybe view) (Result ignored over)
ok =
    Result.onOk


{-| This accessor lets you access values inside the Err variant of a Result.
alias for [`Result.Accessors.onErr`](Result-Accessors#onErr)

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
err : Optic attr view over -> Optic (Result attr ignored) (Maybe view) (Result over ignored)
err =
    Result.onErr


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

    get (L.foo << values) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList

    over (L.foo << values << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    get (L.foo << values << L.bar) dictRecord
    --> [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList

    over (L.foo << values << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
values : Optic attr view over -> Optic (Dict key attr) (Dict key view) (Dict key over)
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
keyed : Optic ( key, attr ) view ( ignored, over ) -> Optic (Dict key attr) (Dict key view) (Dict key over)
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

    get (key "foo" << try << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << try << L.bar) 3 dict
    --> dict

-}
key : String -> Optic (Maybe attr) view (Maybe attr) -> Optic (Dict String attr) view (Dict String attr)
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

    get (keyI 1 << try << L.bar) dict
    --> Just 2

    set (keyI 1) Nothing dict
    --> Dict.remove 1 dict

    set (keyI 0 << try << L.bar) 3 dict
    --> dict

-}
keyI : Int -> Optic (Maybe attr) view (Maybe attr) -> Optic (Dict Int attr) view (Dict Int attr)
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

    keyC : Char -> Optic (Maybe attr) view (Maybe attr) -> Optic (Dict Char attr) view (Dict Char attr)
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
key_ : (comparable -> String) -> comparable -> Optic (Maybe attr) view (Maybe attr) -> Optic (Dict comparable attr) view (Dict comparable attr)
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

    over fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Optic attr view over -> Optic ( attr, ignored ) view ( over, ignored )
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
        |> over fst (\s -> String.toUpper s ++ "!!!")
        |> over snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Optic attr view over -> Optic ( ignored, attr ) view ( ignored, over )
snd =
    Tuple.snd
