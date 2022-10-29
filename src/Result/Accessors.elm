module Result.Accessors exposing (onErr, onOk)

import Base exposing (Relation)


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
onOk : Relation attribute built transformed -> Relation (Result x attribute) built (Maybe transformed)
onOk =
    Base.makeOneToN_ "?" (\fn -> Result.map fn >> Result.toMaybe) Result.map


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
onErr : Relation attribute built transformed -> Relation (Result attribute x) built (Maybe transformed)
onErr =
    let
        getter fn res =
            case res of
                Err e ->
                    Just (fn e)

                _ ->
                    Nothing
    in
    Base.makeOneToN_ "!" getter Result.mapError
