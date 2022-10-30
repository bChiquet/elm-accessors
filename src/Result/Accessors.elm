module Result.Accessors exposing (onErr, onOk)

import Base exposing (Optic)


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

    map (L.foo << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    map (L.qux << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
onOk : Optic attr view over -> Optic (Result ignored attr) (Maybe view) (Result ignored over)
onOk =
    Base.makeOneToN "?" (\fn -> Result.map fn >> Result.toMaybe) Result.map


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

    map (L.foo << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    map (L.qux << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
onErr : Optic attr view over -> Optic (Result attr ignored) (Maybe view) (Result over ignored)
onErr =
    let
        getter fn res =
            case res of
                Err e ->
                    Just (fn e)

                _ ->
                    Nothing
    in
    Base.makeOneToN "!" getter Result.mapError
