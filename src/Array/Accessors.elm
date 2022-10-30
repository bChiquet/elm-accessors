module Array.Accessors exposing (each, each_, at, id)

{-| Array.Accessors

@docs each, each_, at, id

-}

import Array exposing (Array)
import Base exposing (Optic)
import Maybe.Accessors as Maybe


{-| This accessor combinator lets you access values inside Array.

    import Accessors exposing (get, over)
    import Array exposing (Array)
    import Array.Accessors as Array
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    get (L.foo << Array.each << L.bar) arrayRecord
    --> Array.fromList [2, 3, 4]

    over (L.foo << Array.each << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Optic attribute built transformed -> Optic (Array attribute) built (Array transformed)
each =
    Base.makeOneToN "[]" Array.map Array.map


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (get, over, snd)
    import Array exposing (Array)
    import Array.Accessors as Array
    import Lens as L

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


    get (L.foo << Array.each_) arrayRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})] |> Array.fromList

    over (L.foo << Array.each_) multiplyIfGTOne arrayRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}

    get (L.foo << Array.each_ << snd << L.bar) arrayRecord
    --> [2, 3, 4] |> Array.fromList

    over (L.foo << Array.each_ << snd << L.bar) ((+) 1) arrayRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}

-}
each_ : Optic ( Int, attribute ) reachable built -> Optic (Array attribute) reachable (Array built)
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


{-| This accessor combinator lets you access Array indices.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (get, set)
    import Array exposing (Array)
    import Array.Accessors as Array
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    get (Array.at 1) arr
    --> Just { bar = "Things" }

    get (Array.at 9000) arr
    --> Nothing

    get (Array.at 0 << L.bar) arr
    --> Just "Stuff"

    set (Array.at 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (Array.at 9000 << L.bar) "Whatever" arr
    --> arr

-}
at : Int -> Optic v reachable wrap -> Optic (Array v) reachable (Maybe wrap)
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


{-| This accessor combinator lets you access a record with a particular id in an Array.

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Accessors exposing (get, set)
    import Array exposing (Array)
    import Array.Accessors as Array
    import Lens as L

    arr : Array { id : Int, bar : String }
    arr = Array.fromList [{ id = 7, bar = "Stuff" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]

    get (Array.id 1) arr
    --> Just { id = 1, bar = "Things" }

    get (Array.id 9000) arr
    --> Nothing

    get (Array.id 7 << L.bar) arr
    --> Just "Stuff"

    set (Array.id 7 << L.bar) "Whatever" arr
    --> Array.fromList [{ id = 7, bar = "Whatever" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]

    set (Array.id 9000 << L.bar) "Whatever" arr
    --> arr

-}
id : Int -> Optic { m | id : Int } reachable wrap -> Optic (Array { m | id : Int }) reachable (Maybe wrap)
id key =
    Base.makeOneToOne
        ("[" ++ String.fromInt key ++ "]")
        (if key < 0 then
            always Nothing

         else
            Array.filter (\v -> v.id == key)
                >> Array.get 0
        )
        (\fn ->
            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
            Array.foldl
                (\e acc ->
                    case
                        if e.id == key then
                            fn (Just e)

                        else
                            Just e
                    of
                        Just v ->
                            Array.push v acc

                        Nothing ->
                            acc
                )
                Array.empty
        )
        << Maybe.try
