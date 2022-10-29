module Array.Accessors exposing (each, each_, at, id)

{-| Array.Accessors

@docs each, each_, at, id

-}

import Array exposing (Array)
import Base exposing (Relation)
import Maybe.Accessors as Maybe


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
each : Relation attribute built transformed -> Relation (Array attribute) built (Array transformed)
each =
    Base.makeOneToN "[]" Array.map Array.map


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
each_ : Relation ( Int, attribute ) reachable built -> Relation (Array attribute) reachable (Array built)
each_ =
    Base.makeOneToN "#[]"
        (\fn ->
            Array.indexedMap
                (\idx -> Tuple.pair idx >> fn)
        )
        (\fn ->
            Array.indexedMap
                (\idx -> Tuple.pair idx >> fn >> Tuple.second)
        )


at : Int -> Relation v reachable wrap -> Relation (Array v) reachable (Maybe wrap)
at idx =
    Base.makeOneToOne
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
        << Maybe.try


id : Int -> Relation { m | id : Int } reachable wrap -> Relation (Array { m | id : Int }) reachable (Maybe wrap)
id key =
    Base.makeOneToOne
        ("[" ++ String.fromInt key ++ "]")
        (Array.get key)
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            -- so `List.filterMap identity` should be safe
            -- TODO: there's a better way to write this no doubt.
            Array.map
                (\v ->
                    if key == v.id then
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
        << Maybe.try
