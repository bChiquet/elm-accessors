module Accessors.Library exposing (onEach, try, dictEntry)

{-| This library contains common accessors.

@docs onEach, try, dictEntry
-}

import Accessors exposing (Relation, makeOneToOne, makeOneToN)
import Dict exposing (Dict)

{-| This accessor combinator lets you access values inside lists.

    listRecord = { foo = [ {bar = 2}
                         , {bar = 3}
                         , {bar = 4}
                         ]
                 }

    get (foo << onEach << bar) listRecord
    -- returns [2, 3, 4] 

    over (foo << onEach << bar) ((+) 1) listRecord
    -- returns {foo = [{bar = 3}, {bar = 4}, {bar = 5}]}
-}
onEach : Relation super sub wrap -> Relation (List super) sub (List wrap)
onEach = makeOneToN List.map List.map


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
try : Relation super sub wrap -> Relation (Maybe super) sub (Maybe wrap)
try = makeOneToN Maybe.map Maybe.map

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
dictEntry : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
dictEntry key =
    makeOneToOne (Dict.get key) (Dict.update key)
