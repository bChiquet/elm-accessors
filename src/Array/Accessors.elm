module Array.Accessors exposing (each)

{-| Array.Accessors

@docs each, each_, at, id

-}

import Array exposing (Array)
import Base exposing (Optic, Traversal)
import Maybe.Accessors as Maybe


{-| This accessor combinator lets you access values inside Array.

    import Accessors exposing (get, map)
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

    map (L.foo << Array.each << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
each : Optic pr ls a b x y -> Traversal (Array a) (Array b) x y
each =
    Base.traversal "[]" Array.toList Array.map



--{-| This accessor lets you traverse a list including the index of each element
--    import Accessors exposing (get, map, snd)
--    import Array exposing (Array)
--    import Array.Accessors as Array
--    import Lens as L
--    arrayRecord : { foo : Array { bar : Int } }
--    arrayRecord = { foo = [ {bar = 2}
--                          , {bar = 3}
--                          , {bar = 4}
--                          ] |> Array.fromList
--                  }
--    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
--    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
--        if idx > 0 then
--            ( idx, { bar = bar * 10 } )
--        else
--            (idx, rec)
--    get (L.foo << Array.each_) arrayRecord
--    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})] |> Array.fromList
--    map (L.foo << Array.each_) multiplyIfGTOne arrayRecord
--    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}
--    get (L.foo << Array.each_ << snd << L.bar) arrayRecord
--    --> [2, 3, 4] |> Array.fromList
--    map (L.foo << Array.each_ << snd << L.bar) ((+) 1) arrayRecord
--    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}
---}
--each_ =
--    Base.traversal "#[]"
--        (\fn ->
--            Array.indexedMap
--                (\idx -> Tuple.pair idx >> fn)
--                >> Array.toList
--        )
--        (\fn ->
--            Array.indexedMap
--                (\idx -> Tuple.pair idx >> fn >> Tuple.second)
--        )
--{-| This accessor combinator lets you access Array indices.
--In terms of accessors, think of Dicts as records where each field is a Maybe.
--    import Accessors exposing (get, set)
--    import Array exposing (Array)
--    import Array.Accessors as Array
--    import Lens as L
--    arr : Array { bar : String }
--    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]
--    get (Array.at 1) arr
--    --> Just { bar = "Things" }
--    get (Array.at 9000) arr
--    --> Nothing
--    get (Array.at 0 << L.bar) arr
--    --> Just "Stuff"
--    set (Array.at 0 << L.bar) "Whatever" arr
--    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]
--    set (Array.at 9000 << L.bar) "Whatever" arr
--    --> arr
---}
--at : Int -> Optic over view over -> Optic (Array over) (Maybe view) (Array over)
--at idx =
--    Base.lens
--        ("[" ++ String.fromInt idx ++ "]")
--        (Array.get idx)
--        (\fn ->
--            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
--            -- so `List.filterMap identity` should be safe
--            -- TODO: there's a better way to write this no doubt.
--            Array.foldl
--                (\el ( idx_, acc ) ->
--                    ( idx_ + 1
--                    , case
--                        if idx == idx_ then
--                            fn (Just el)
--                        else
--                            Just el
--                      of
--                        Just v ->
--                            Array.push v acc
--                        Nothing ->
--                            acc
--                    )
--                )
--                ( 0, Array.empty )
--                >> Tuple.second
--        )
--        << Maybe.try
--{-| This accessor combinator lets you access a record with a particular id in an Array.
--In terms of accessors, think of Dicts as records where each field is a Maybe.
--    import Accessors exposing (get, set)
--    import Array exposing (Array)
--    import Array.Accessors as Array
--    import Lens as L
--    arr : Array { id : Int, bar : String }
--    arr = Array.fromList [{ id = 7, bar = "Stuff" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]
--    get (Array.id 1) arr
--    --> Just { id = 1, bar = "Things" }
--    get (Array.id 9000) arr
--    --> Nothing
--    get (Array.id 7 << L.bar) arr
--    --> Just "Stuff"
--    set (Array.id 7 << L.bar) "Whatever" arr
--    --> Array.fromList [{ id = 7, bar = "Whatever" }, { id = 1, bar =  "Things" }, { id = 5, bar = "Woot" }]
--    set (Array.id 9000 << L.bar) "Whatever" arr
--    --> arr
---}
--id : Int -> Optic { attr | id : Int } view { attr | id : Int } -> Optic (Array { attr | id : Int }) (Maybe view) (Array { attr | id : Int })
--id key =
--    Base.lens
--        ("[" ++ String.fromInt key ++ "]")
--        (if key < 0 then
--            always Nothing
--         else
--            Array.filter (\v -> v.id == key)
--                >> Array.get 0
--        )
--        (\fn ->
--            -- NOTE: `<< try` at the end ensures we can't delete any existing keys
--            Array.foldl
--                (\e acc ->
--                    case
--                        if e.id == key then
--                            fn (Just e)
--                        else
--                            Just e
--                    of
--                        Just v ->
--                            Array.push v acc
--                        Nothing ->
--                            acc
--                )
--                Array.empty
--        )
--        << Maybe.try
