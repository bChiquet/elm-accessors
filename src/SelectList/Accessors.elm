module SelectList.Accessors exposing (each, each_, selected)

{-| This module exposes some helpers for "miyamoen/select-list"

@docs each, each_, selected

-}

import Base exposing (Relation)
import SelectList exposing (SelectList)


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    get (L.foo << SL.each << L.bar) listRecord
    --> SelectList.fromLists [1] 2 [3, 4]

    over (L.foo << SL.each << L.bar) ((+) 1) listRecord
    --> { foo = SelectList.fromLists [{ bar = 2 }] { bar = 3 } [{ bar = 4 }, { bar = 5 }] }

-}
each : Relation attribute built transformed -> Relation (SelectList attribute) built (SelectList transformed)
each =
    Base.makeOneToN_ ":[_]" SelectList.map SelectList.map


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)


    get (L.foo << SL.each_) listRecord
    --> SelectList.fromLists [(0, {bar = 1})] (1, {bar = 2}) [(2, {bar = 3}), (3, {bar = 4})]

    over (L.foo << SL.each_) multiplyIfGTOne listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 20 } [{ bar = 30 }, { bar = 40 }] }

    get (L.foo << SL.each_ << snd << L.bar) listRecord
    --> SelectList.fromLists [1] 2 [3, 4]

    over (L.foo << SL.each_ << snd << L.bar) ((+) 1) listRecord
    --> {foo = SelectList.fromLists [{bar = 2}] {bar = 3} [{bar = 4}, {bar = 5}]}

-}
each_ : Relation ( Int, attribute ) reachable built -> Relation (SelectList attribute) reachable (SelectList built)
each_ =
    Base.makeOneToN_ "[#]"
        (\fn ls ->
            let
                ( before, current, after ) =
                    SelectList.toTuple ls

                currentIdx : Int
                currentIdx =
                    SelectList.index ls
            in
            SelectList.fromLists (List.indexedMap (\idx -> Tuple.pair idx >> fn) before)
                (fn ( currentIdx, current ))
                (List.indexedMap (\idx -> Tuple.pair (idx + (currentIdx + 1)) >> fn) after)
        )
        (\fn ls ->
            let
                ( before, current, after ) =
                    SelectList.toTuple ls

                currentIdx : Int
                currentIdx =
                    SelectList.index ls
            in
            SelectList.fromLists (List.indexedMap (\idx -> Tuple.pair idx >> fn >> Tuple.second) before)
                (fn ( currentIdx, current ) |> Tuple.second)
                (List.indexedMap (\idx -> Tuple.pair (idx + (currentIdx + 1)) >> fn >> Tuple.second) after)
        )


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)

    get (L.foo << SL.selected << L.bar) listRecord
    --> 2

    set (L.foo << SL.selected << L.bar) 37 listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 37 } [{ bar = 3 }, { bar = 4 }] }

    over (L.foo << SL.selected << L.bar) ((*) 10) listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 20 } [{ bar = 3 }, { bar = 4 }] }

-}
selected : Relation attribute reachable built -> Relation (SelectList attribute) reachable built
selected =
    Base.makeOneToOne_ "[^]" SelectList.selected SelectList.updateSelected
