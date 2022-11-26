module Base exposing
    ( Optic(..), Traversal, Lens, Prism, Iso
    , SimpleOptic, SimpleTraversal, SimpleLens, SimplePrism, SimpleIso
    , traversal, lens, prism, iso
    , ixd, from
    , get, all, try, has, map, set, new, name
    )

{-|


# Optics

@docs Optic, Traversal, Lens, Prism, Iso
@docs SimpleOptic, SimpleTraversal, SimpleLens, SimplePrism, SimpleIso


# Build your own accessors

Accessors are built using these functions:

@docs traversal, lens, prism, iso


# Accessor Lifters for Indexed operations

@docs ixd, from


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


{-| The lens is "not a prism".
-}
type alias Lens ls s t a b =
    Optic Never ls s t a b


{-| The prism is "not a lens".
-}
type alias Prism pr s t a b =
    Optic pr Never s t a b


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal s t a b =
    Optic Never Never s t a b


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
lens n sa sbt sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f s =
            s |> sa |> f |> sbt s
    in
    Optic
        { list = sa >> List.singleton
        , view = sa
        , make = void "Can't call `make` with a Lens"
        , over = over_
        , name = n
        }
        |> dot sub


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
prism n bt sta sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            sta >> Result.map (f >> bt) >> mergeResult
    in
    Optic
        { view = void "Can't call `view` with a Prism"
        , list =
            sta
                >> Result.map List.singleton
                >> Result.withDefault []
        , make = bt
        , over = over_
        , name = n
        }
        |> dot sub


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
traversal n sa abst sub =
    Optic
        { view = void "Can't call `view` with a Traversal"
        , make = void "Can't call `make` with a Traversal"
        , list = sa
        , over = abst
        , name = n
        }
        |> dot sub


{-| An isomorphism constructor.
-}
iso : String -> (s -> a) -> (b -> t) -> Optic pr ls a b x y -> Iso pr ls s t x y
iso n sa bt sub =
    let
        over_ : (a -> b) -> s -> t
        over_ f =
            bt << f << sa
    in
    Optic
        { view = sa
        , list = sa >> List.singleton
        , make = bt
        , over = over_
        , name = n
        }
        |> dot sub


dot : Optic any thing a b x y -> Optic pr ls s t a b -> Optic pr ls s t x y
dot (Optic attribute) (Optic structure) =
    Optic
        { list = structure.list >> List.concatMap attribute.list
        , view = structure.view >> attribute.view
        , make = structure.make << attribute.make
        , over = structure.over << attribute.over
        , name = structure.name ++ attribute.name
        }


ixd :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> (Optic pr ls a b x y -> Traversal ( ix, s ) t x y)
ixd p =
    traversal (name p)
        (\( _, b ) -> all p b)
        (\fn -> Tuple.mapSecond (map p fn) >> Tuple.second)


from :
    (Optic pr ls a b a b -> Iso pr ls s t a b)
    -> (Optic pr ls t s t s -> Iso pr ls b a t s)
from accessor =
    let
        i =
            id
                |> accessor
                |> internal
    in
    iso (String.reverse i.name)
        i.make
        i.view


id : Optic pr ls a b a b
id =
    Optic
        { view = identity
        , make = identity
        , over = identity
        , list = List.singleton
        , name = ""
        }



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
    (Optic pr ls a b a b -> Optic pr Y s t a b)
    -> s
    -> a
get accessor =
    (id
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
has accessor s =
    try accessor s /= Nothing


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
    (id |> accessor |> internal).list >> List.head


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
    (id |> accessor |> internal).list


set :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> b
    -> (s -> t)
set accessor attr =
    (id |> accessor |> internal).over
        (\_ -> attr)


map :
    (Optic pr ls a b a b -> Optic pr ls s t a b)
    -> (a -> b)
    -> s
    -> t
map accessor =
    (id |> accessor |> internal).over


{-| Use prism to reconstruct.
-}
new : (Optic pr ls a b a b -> Optic Y ls s t a b) -> b -> t
new accessor =
    (id |> accessor |> internal).make


{-| -}
name : (Optic pr ls a b a b -> Optic pr ls s t a b) -> String
name accessor =
    (id |> accessor |> internal).name



-- Helper


void : String -> any -> runTimeError
void s _ =
    let
        unoptimizedRecursion : String -> runTimeError
        unoptimizedRecursion str =
            unoptimizedRecursion str
    in
    unoptimizedRecursion s
