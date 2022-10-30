module Maybe.Accessors exposing (def, or, try)

import Base exposing (Optic)


{-| This accessor combinator lets you access values inside Maybe.

    import Base exposing (view, over)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Maybe.Accessors as Maybe
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
    maybeRecord = { foo = Just { bar = 2 }
                  , qux = Nothing
                  }

    view (L.foo << Maybe.try << L.bar) maybeRecord
    --> Just 2

    view (L.qux << Maybe.try << L.bar) maybeRecord
    --> Nothing

    over (L.foo << Maybe.try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 3}, qux = Nothing}

    over (L.qux << Maybe.try << L.bar) ((+) 1) maybeRecord
    --> {foo = Just {bar = 2}, qux = Nothing}

-}
try : Optic attr view over -> Optic (Maybe attr) (Maybe view) (Maybe over)
try =
    Base.makeOneToN "?" Maybe.map Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Base exposing (view)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Maybe.Accessors as Maybe
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    view (Dict.at "foo" << Maybe.def {bar = 0}) dict
    --> {bar = 2}

    view (Dict.at "baz" << Maybe.def {bar = 0}) dict
    --> {bar = 0}

    -- NOTE: The following do not compile :thinking:
    --view (Dict.at "foo" << Maybe.try << L.bar << Maybe.def 0) dict
    ----> 2

    --view (Dict.at "baz" << Maybe.try << L.bar << Maybe.def 0) dict
    ----> 0

-}
def : attr -> Optic attr view over -> Optic (Maybe attr) view (Maybe over)
def d =
    Base.makeOneToN "??"
        (\f -> Maybe.withDefault d >> f)
        Maybe.map


{-| This accessor combinator lets you provide a default value for otherwise failable compositions

    import Base exposing (view)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Maybe.Accessors as Maybe
    import Lens as L

    dict : Dict String {bar : Int}
    dict =
        Dict.fromList [("foo", {bar = 2})]

    -- NOTE: Use `def` for this.
    --view (Dict.at "foo" << Maybe.or {bar = 0}) dict
    ----> {bar = 2}

    --view (Dict.at "baz" << Maybe.or {bar = 0}) dict
    ----> {bar = 0}

    view ((Dict.at "foo" << try << L.bar) |> Maybe.or 0) dict
    --> 2

    view ((Dict.at "baz" << try << L.bar) |> Maybe.or 0) dict
    --> 0

-}
or :
    attr
    -> (Optic attr attr attrOver -> Optic value (Maybe attr) over)
    -> Optic attr attrView attrOver
    -> Optic value attrView over
or d l =
    Base.makeOneToOne "||"
        (Base.view l >> Maybe.withDefault d)
        (Base.over l)
