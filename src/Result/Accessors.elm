module Result.Accessors exposing (ok_, err_)

{-|

@docs ok_, err_

-}

import Base exposing (Optic, Prism)
import Result exposing (Result(..))


{-| This accessor lets you access values inside the Ok variant of a Result.

    import Accessors exposing (..)
    import Result.Accessors as Result
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << Result.ok_ << L.bar) maybeRecord
    --> Just 2

    try (L.qux << Result.ok_ << L.bar) maybeRecord
    --> Nothing

    map (L.foo << Result.ok_ << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    map (L.qux << Result.ok_ << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok_ : Optic pr ls a b x y -> Prism pr (Result ignored a) (Result ignored b) x y
ok_ =
    Base.prism ".?[Ok]" Ok (unwrap (Err >> Err) Ok)


{-| This accessor lets you access values inside the Err variant of a Result.

    import Accessors exposing (..)
    import Result.Accessors as Result
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << Result.err_) maybeRecord
    --> Nothing

    try (L.qux << Result.err_) maybeRecord
    --> Just "Not an Int"

    map (L.foo << Result.err_) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    map (L.qux << Result.err_) String.toUpper maybeRecord
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
