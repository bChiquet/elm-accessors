module Dict.Accessors exposing (each, each_, at, id, at_)

{-| Dict.Accessors

@docs each, each_, at, id, at_

-}

import Base exposing (Lens, Optic, Traversal)
import Dict exposing (Dict)


{-| each: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Lens as L

    dictRecord : { foo : Dict String { bar : Int } }
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    all (L.foo << Dict.each) dictRecord
    --> [{bar = 2}, {bar = 3}, {bar = 4}]

    map (L.foo << Dict.each << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    all (L.foo << Dict.each << L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << Dict.each << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
each : Optic pr ls a b x y -> Traversal (Dict key a) (Dict key b) x y
each =
    Base.traversal "{_}"
        Dict.values
        (\fn -> Dict.map (\_ -> fn))


{-| keyed: This accessor lets you traverse a Dict including the index of each element

    import Accessors exposing (..)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Lens as L

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


    all (L.foo << Dict.each_) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})]

    map (L.foo << Dict.each_) multiplyIfA dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList}

    all (L.foo << Dict.each_ << ixL L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << Dict.each_ << ixL L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
each_ : Optic pr ls ( a, b ) c x y -> Traversal (Dict a b) (Dict a c) x y
each_ =
    Base.traversal "{_}"
        Dict.toList
        (\fn -> Dict.map (\idx -> Tuple.pair idx >> fn))


{-| at: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (..)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (Dict.at "foo") dict
    --> Just {bar = 2}

    get (Dict.at "baz") dict
    --> Nothing

    try (Dict.at "foo" << just_ << L.bar) dict
    --> Just 2

    set (Dict.at "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (Dict.at "baz" << just_ << L.bar) 3 dict
    --> dict

-}
at : String -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict String a) (Dict String a) x y
at =
    at_ identity


{-| id: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (..)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Lens as L

    dict : Dict Int {bar : Int}
    dict = Dict.fromList [(1, {bar = 2})]

    get (Dict.id 1) dict
    --> Just {bar = 2}

    get (Dict.id 0) dict
    --> Nothing

    try (Dict.id 1 << just_ << L.bar) dict
    --> Just 2

    set (Dict.id 1) Nothing dict
    --> Dict.remove 1 dict

    set (Dict.id 0 << just_ << L.bar) 3 dict
    --> dict

-}
id : Int -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict Int a) (Dict Int a) x y
id =
    at_ String.fromInt


{-| `at_`: NON-structure preserving accessor over Dict's

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (..)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Lens as L

    dict : Dict Char {bar : Int}
    dict = Dict.fromList [('C', {bar = 2})]

    atC : Char -> Optic pr ls (Maybe {bar : Int}) (Maybe {bar : Int}) x y -> Lens ls (Dict Char {bar : Int}) (Dict Char {bar : Int}) x y
    atC =
        Dict.at_ String.fromChar

    get (atC 'C') dict
    --> Just {bar = 2}

    get (atC 'Z') dict
    --> Nothing

    try (atC 'C' << just_ << L.bar) dict
    --> Just 2

    set (atC 'C') Nothing dict
    --> Dict.remove 'C' dict

    set (atC 'Z' << just_ << L.bar) 3 dict
    --> dict

-}
at_ : (comparable -> String) -> comparable -> Optic pr ls (Maybe a) (Maybe a) x y -> Lens ls (Dict comparable a) (Dict comparable a) x y
at_ toS k =
    Base.lens ("{" ++ toS k ++ "}")
        (Dict.get k)
        (\rec val -> Dict.update k (always val) rec)
