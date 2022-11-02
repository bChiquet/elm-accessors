module Base exposing
    ( Optic(..), Traversal, Lens, Prism, Iso, Y
    , SimpleOptic, SimpleTraversal, SimpleLens, SimplePrism, SimpleIso
    , traversal, lens, prism, iso
    , ixT, ixL, ixP, from
    , get, all, try, has, map, set, new, name
    , internal
    )

{-|


# Optics

@docs Optic, Traversal, Lens, Prism, Iso, Y
@docs SimpleOptic, SimpleTraversal, SimpleLens, SimplePrism, SimpleIso


# Build your own accessors

Accessors are built using these functions:

@docs traversal, lens, prism, iso


# Accessor Lifters for Indexed operations

@docs ixT, ixL, ixP, from


# Actions

@docs get, all, try, has, map, set, new, name

-}


{-| A `Optic value view over` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Optic pr ls s t a b
    = Optic (Internal s t a b)


type alias Internal s t a b =
    { view : s -> a
    , make : b -> t
    , over : (a -> b) -> s -> t
    , list : s -> List a
    , name : String
    }


internal : Optic pr ls s t a b -> Internal s t a b
internal (Optic i) =
    i


{-| Use this type as replacement for `pr`/`ls` variable when they are in the
signature of the function that calls any of requiring eliminators (`get`/
`review`/`is`).
-}
type alias Y =
    ()


{-| Type-level "no". Is a 0 type.
-}
type N
    = N Never


{-| The lens is "not a prism".
-}
type alias Lens ls s t a b =
    Optic N ls s t a b


{-| The prism is "not a lens".
-}
type alias Prism pr s t a b =
    Optic pr N s t a b


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal s t a b =
    Optic N N s t a b


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso pr ls s t a b =
    Optic pr ls s t a b


{-| `Optic` that cannot change type of the object.
-}
type alias SimpleOptic pr ls s a =
    Optic pr ls s s a a


{-| `Lens` that cannot change type of the object.
-}
type alias SimpleLens ls s a =
    Lens ls s s a a


{-| `Prism` that cannot change type of the object.
-}
type alias SimplePrism pr s a =
    Prism pr s s a a


{-| `Traversal` that cannot change type of the object.
-}
type alias SimpleTraversal s a =
    Traversal s s a a


{-| `Iso` that cannot change type of the object.
-}
type alias SimpleIso pr ls s a =
    Iso pr ls s s a a


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Optic attr attrView attrOver -> Optic { rec | foo : attr } view over
    foo =
        Accessors.lens ".foo" .foo (\rec new -> { rec | foo = new })

-}
lens :
    String
    -> (s -> a)
    -> (s -> b -> t)
    -- Any optic composed with a Lens becomes "at least a Lens".
    -> (Optic pr ls a b x y -> Lens ls s t x y)
lens n sa sbt (Optic sub) =
    let
        over_ : (a -> b) -> s -> t
        over_ f s =
            s |> sa |> f |> sbt s
    in
    Optic
        { list = sa >> List.singleton >> List.concatMap sub.list
        , view = sub.view << sa
        , make = \_ -> void "Can't call `make` with a Lens"
        , over = sub.over >> over_
        , name = n ++ sub.name
        }


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out `a`.

-}
prism :
    String
    -> (b -> t)
    -> (s -> Result t a)
    -> (Optic pr ls a b x y -> Prism pr s t x y)
prism n bt sta (Optic sub) =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            sta >> Result.map (f >> bt) >> mergeResult
    in
    Optic
        { view = \_ -> void "Can't call `view` with a Prism"
        , list =
            sta
                >> Result.map (\a -> [ a ])
                >> Result.withDefault []
                >> List.concatMap sub.list
        , make = bt << sub.make
        , over = sub.over >> over_
        , name = n ++ sub.name
        }


mergeResult : Result a a -> a
mergeResult r =
    case r of
        Ok a ->
            a

        Err a ->
            a


traversal :
    String
    -> (s -> List a)
    -> ((a -> b) -> s -> t)
    -> (Optic pr ls a b x y -> Traversal s t x y)
traversal n sa abst (Optic sub) =
    Optic
        { view = \_ -> void "Can't call `view` with a Traversal"
        , make = \_ -> void "Can't call `make` with a Traversal"
        , list = sa >> List.concatMap sub.list
        , over = sub.over >> abst
        , name = n ++ sub.name
        }


{-| An isomorphism constructor.
-}
iso : String -> (s -> a) -> (b -> t) -> Optic pr ls a b x y -> Iso pr ls s t x y
iso n sa bt (Optic sub) =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            bt << f << sa
    in
    Optic
        { view = sub.view << sa
        , list = sa >> List.singleton >> List.concatMap sub.list
        , make = bt << sub.make
        , over = sub.over >> over_
        , name = n ++ sub.name
        }


ixT :
    (Optic pr ls x y x y -> Optic pr ls b t x y)
    -> Optic a c x y d e
    -> Traversal ( idx, b ) t d e
ixT t =
    traversal (name t)
        (\( _, rec ) -> all t rec)
        (\fn -> Tuple.mapSecond (map t fn) >> Tuple.second)


ixL :
    (Optic pr Y x z x z -> Optic pr Y a b x z)
    -> Optic c ls x z d y
    -> Lens ls ( idx, a ) b d y
ixL l =
    lens (name l)
        (\( _, rec ) -> get l rec)
        (\rec val -> Tuple.mapSecond (set l val) rec |> Tuple.second)


ixP :
    (Optic Y ls value rte value rte -> Optic Y ls b t value rte)
    -> Optic pr a value rte x y
    -> Prism pr ( idx, b ) t x y
ixP p =
    prism (name p)
        (\b -> new p b)
        (\( _, s ) ->
            case try p s of
                Just v ->
                    Ok v

                Nothing ->
                    Err (map p (\_ -> void "make an ix") s)
        )



-- from : (Iso pr ls a b a b -> Iso pr ls s t a b) -> (Iso pr ls t s t s -> Iso pr ls b a t s)


from :
    (Optic pr ls a b a b -> Iso pr ls s t a b)
    -> (Optic pr ls t s t s -> Iso pr ls b a t s)
from i =
    iso (String.reverse (name i))
        (new i)
        (get i)



-- {-| Optical composition.
-- -}
-- o : Optic pr ls s t a b -> Optic pr ls a b x y -> Optic pr ls s t x y
-- o (Optic f) (Optic g) =
--     Optic
--         { list = f.list >> List.concatMap g.list
--         , view = \( y, s ) -> g.view ( y, f.view ( y, s ) )
--         , make = \( y, b ) -> f.make ( y, g.make ( y, b ) )
--         , over = g.over >> f.over
--         , name = f.name ++ g.name
--         }


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Optic attr view over -> Optic (List attr) view (List over)
    each =
        makeOneToN "[]"
            List.map
            List.map

-}



-- traversal :
--     String
--     -> ((attr -> attrView) -> (value -> view))
--     -> ((attr -> attrOver) -> (value -> over))
--     -> Optic attr attrView attrOver
--     -> Optic value view over
-- traversal n viewAttr overAttr (Optic sub) =
--     Optic
--         { view = viewAttr sub.view
--         , over = overAttr sub.over
--         , name = n ++ sub.name
--         }
-- Actions


get :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> a
get accessor =
    (Optic
        { make = \_ -> void "`make` should never be called from `get`"
        , over = \_ -> void "`over` should never be called from `get`"
        , list = \_ -> void "`list` should never be called from `get`"
        , view = identity
        , name = ""
        }
        |> accessor
        |> internal
    ).view


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> has try
    --> True

    Nothing
        |> has try
    --> False

    ["Stuff", "things"]
        |> has (at 2)
    --> False

    ["Stuff", "things"]
        |> has (at 0)
    --> True

-}
has :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> Bool
has accessor =
    (Optic
        { make = \_ -> void "`make` should never be called from `has`"
        , over = \_ -> void "`over` should never be called from `has`"
        , view = \_ -> void "`view` should never be called from `has`"
        , list = List.singleton
        , name = ""
        }
        |> accessor
        |> internal
    ).list
        >> List.head
        >> (/=) Nothing


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    ["Stuff", "things"]
        |> try (at 2)
    --> Nothing

    ["Stuff", "things"]
        |> try (at 0)
    --> Just "Stuff"

-}
try :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> Maybe a
try accessor =
    (Optic
        { make = \_ -> void "`make` should never be called from `try`"
        , over = \_ -> void "`over` should never be called from `try`"
        , view = \_ -> void "`view` should never be called from `try`"
        , list = List.singleton
        , name = ""
        }
        |> accessor
        |> internal
    ).list
        >> List.head


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just "Stuff"
        |> all just_
    --> ["Stuff"]

    Nothing
        |> all just_
    --> []

-}
all :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> s
    -> List a
all accessor =
    (Optic
        { make = \_ -> void "`make` should never be called from `all`"
        , over = \_ -> void "`over` should never be called from `all`"
        , view = \_ -> void "`view` should never be called from `all`"
        , list = List.singleton
        , name = ""
        }
        |> accessor
        |> internal
    ).list


set :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> b
    -> (s -> t)
set accessor attr =
    (Optic
        { view = \_ -> void "`view` should never be called from `set`"
        , make = \_ -> void "`make` should never be called from `set`"
        , list = \_ -> void "`list` should never be called from `set`"
        , over = identity
        , name = ""
        }
        |> accessor
        |> internal
    ).over
        (\_ -> attr)


map :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> (a -> b)
    -> s
    -> t
map accessor change =
    (Optic
        { view = \_ -> void "`view` should never be called from `over`"
        , make = \_ -> void "`make` should never be called from `over`"
        , list = \_ -> void "`list` should never be called from `over`"
        , over = identity
        , name = ""
        }
        |> accessor
        |> internal
    ).over
        change


{-| Use prism to reconstruct.
-}
new : (Optic pr ls a b a b -> Optic pr ls s t a b) -> b -> t
new accessor =
    (Optic
        { view = \_ -> void "`view` should never be called from `name`"
        , list = \_ -> void "`list` should never be called from `name`"
        , over = \_ -> void "`over` should never be called from `name`"
        , make = \b -> b
        , name = ""
        }
        |> accessor
        |> internal
    ).make


name : (Optic pr ls a b x y -> Optic pr ls s t a b) -> String
name accessor =
    (Optic
        { view = \_ -> void "`view` should never be called from `name`"
        , make = \_ -> void "`make` should never be called from `name`"
        , list = \_ -> void "`list` should never be called from `name`"
        , over = \_ -> void "`over` should never be called from `name`"
        , name = ""
        }
        |> accessor
        |> internal
    ).name



-- Helper


void : String -> runTimeError
void s =
    let
        unoptimizedRecursion : String -> runTimeError
        unoptimizedRecursion str =
            unoptimizedRecursion str
    in
    unoptimizedRecursion s
