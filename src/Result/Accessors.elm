module Result.Accessors exposing (err_, ok_)

import Base exposing (Optic, Prism)
import Result exposing (Result(..))


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
ok_ : Optic pr ls a b x y -> Prism pr (Result ignored a) (Result ignored b) x y
ok_ =
    Base.prism ".?[Ok]" Ok (unwrap (Err >> Err) Ok)


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
err_ : Optic pr ls a b x y -> Prism pr (Result a ignored) (Result b ignored) x y
err_ =
    Base.prism ".?[Err]" Err (unwrap Ok (Ok >> Err))


unwrap : (e -> x) -> (a -> x) -> Result e a -> x
unwrap onE onA r =
    case r of
        Ok a ->
            onA a

        Err e ->
            onE e
