module Maybe.Accessors exposing (def, or, try)

import Base exposing (Optic)


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    get (L.foo << try << L.bar) maybeRecord
    --> Just 2

    get (L.qux << try << L.bar) maybeRecord
    --> Nothing

    over (L.foo << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (L.qux << try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Optic attribute built transformed -> Optic (Maybe attribute) built (Maybe transformed)
try =
    Base.makeOneToN "?" Maybe.map Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    get (key "foo" << def {bar = 0}) dict
    --> {bar = 2}

    get (key "baz" << def {bar = 0}) dict
    --> {bar = 0}

    -- NOTE: The following do not compile :thinking:
    --get (key "foo" << try << L.bar << def 0) dict
    ----> 2

    --get (key "baz" << try << L.bar << def 0) dict
    ----> 0

-}
def : attribute -> Optic attribute reachable wrap -> Optic (Maybe attribute) reachable wrap
def d =
    Base.makeOneToN "??"
        (\f -> Maybe.withDefault d >> f)
        Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Dict exposing (Dict)
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    -- NOTE: Use `def` for this.
    --get (key "foo" << or {bar = 0}) dict
    ----> {bar = 2}

    --get (key "baz" << or {bar = 0}) dict
    ----> {bar = 0}

    get ((key "foo" << try << L.bar) |> or 0) dict
    --> 2

    get ((key "baz" << try << L.bar) |> or 0) dict
    --> 0

-}
or :
    attribute
    -> (Optic attribute attribute attribute -> Optic structure attribute (Maybe attribute))
    -> (Optic attribute other attribute -> Optic structure other attribute)
or d l =
    Base.makeOneToOne "||"
        (Base.view l >> Maybe.withDefault d)
        (Base.over l)
