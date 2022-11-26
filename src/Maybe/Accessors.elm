module Maybe.Accessors exposing (just_)

{-|

@docs just_

-}

import Base exposing (Optic, Prism)


{-| This accessor combinator lets you access values inside Maybe.
see [`try_`](Maybe-Accessors#try_) for a flattening lens.

    import Base exposing (try, map)
    import Dict exposing (Dict)
    import Dict.Accessors as Dict
    import Maybe.Accessors as Maybe
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
                  , qux = Nothing
                  }

    try (L.foo << Maybe.just_ << L.bar << Maybe.just_ << L.stuff) maybeRecord
    --> Just (Just 2)

    try (L.qux << Maybe.just_ << L.bar) maybeRecord
    --> Nothing

    map (L.foo << Maybe.just_ << L.bar << Maybe.just_ << L.stuff << Maybe.just_) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}

    map (L.qux << Maybe.just_ << L.bar << Maybe.just_) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}

-}
just_ : Optic pr ls a b x y -> Prism pr (Maybe a) (Maybe b) x y
just_ =
    Base.prism "?" Just (Result.fromMaybe Nothing)



--{-| This accessor combinator lets you access values inside Maybe.
--see [`try`](Maybe-Accessors#try) for a NON-flattening lens.
--    import Base exposing (get, map)
--    import Dict exposing (Dict)
--    import Dict.Accessors as Dict
--    import Maybe.Accessors as Maybe
--    import Lens as L
--    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
--    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
--                  , qux = Nothing
--                  }
--    get (L.foo << Maybe.try_ << L.bar << Maybe.try_ << L.stuff) maybeRecord
--    --> Just 2
--    get (L.qux << Maybe.try_ << L.bar) maybeRecord
--    --> Nothing
--    map (L.foo << Maybe.try_ << L.bar << Maybe.try_ << L.stuff << Maybe.try_) ((+) 1) maybeRecord
--    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}
--    map (L.qux << Maybe.try_ << L.bar << Maybe.try_) ((+) 1) maybeRecord
--    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}
---}
-- try_ : Optic attr (Maybe view) over -> Optic (Maybe attr) (Maybe view) (Maybe over)
-- try_ =
--     Base.traversal "?" Maybe.andThen Maybe.map
--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--    import Base exposing (get)
--    import Dict exposing (Dict)
--    import Dict.Accessors as Dict
--    import Maybe.Accessors as Maybe
--    import Lens as L
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    get (Dict.at "foo" << Maybe.def {bar = 0}) dict
--    --> {bar = 2}
--    get (Dict.at "baz" << Maybe.def {bar = 0}) dict
--    --> {bar = 0}
--    -- NOTE: The following do not compile :thinking:
--    --get (Dict.at "foo" << Maybe.try << L.bar << Maybe.def 0) dict
--    ----> 2
--    --get (Dict.at "baz" << Maybe.try << L.bar << Maybe.def 0) dict
--    ----> 0
---}
-- def : attr -> Optic attr view over -> Optic (Maybe attr) view (Maybe over)
-- def d =
--     Base.traversal "??"
--         (\f -> Maybe.withDefault d >> f)
--         Maybe.map
--{-| This accessor combinator lets you provide a default value for otherwise failable compositions
--    import Base exposing (get)
--    import Dict exposing (Dict)
--    import Dict.Accessors as Dict
--    import Maybe.Accessors as Maybe
--    import Lens as L
--    dict : Dict String {bar : Int}
--    dict =
--        Dict.fromList [("foo", {bar = 2})]
--    -- NOTE: Use `def` for this.
--    --get (Dict.at "foo" << Maybe.or {bar = 0}) dict
--    ----> {bar = 2}
--    --get (Dict.at "baz" << Maybe.or {bar = 0}) dict
--    ----> {bar = 0}
--    get ((Dict.at "foo" << try << L.bar) |> Maybe.or 0) dict
--    --> 2
--    get ((Dict.at "baz" << try << L.bar) |> Maybe.or 0) dict
--    --> 0
---}
-- or :
--     attr
--     -> (Optic attr attr attrOver -> Optic value (Maybe attr) over)
--     -> Optic attr attrView attrOver
--     -> Optic value attrView over
-- or d l =
--     Base.lens "||"
--         (Base.get l >> Maybe.withDefault d)
--         (Base.map l)
