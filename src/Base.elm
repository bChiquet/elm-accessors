module Base exposing
    ( Optic, Setable
    , makeOneToOne, makeOneToN
    , is, name, over, set, view
    )

{-|


# Optics

@docs Optic, Accessor, Lens, Lens_, Setable


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

-}


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Optic structure view over
    = Optic (Internal structure view over)


type alias Internal structure view over =
    { view : structure -> view
    , over : structure -> over
    , name : String
    }


internal : Optic structure view over -> Internal structure view over
internal (Optic i) =
    i


type alias Getter value view over attr attrOver =
    Optic attr attr attrOver -> Optic value view over


{-| Type of a composition of accessors that `set` can be called with.
-}
type alias Setable value view over attrOver =
    Optic view view attrOver -> Optic value view over


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    String
    -- -> (value -> view)
    -- -> ((view -> attrOver) -> value -> over)
    -> (value -> attr)
    -> ((attr -> attrOver) -> (value -> over))
    -> (Optic attr attrView attrOver -> Optic value attrView over)
makeOneToOne n viewSuper overSuper =
    makeOneToN n
        (\viewAttr -> viewSuper >> viewAttr)
        overSuper


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN "[]"
            List.map
            List.map

-}
makeOneToN :
    String
    -> ((attr -> attrView) -> (value -> view))
    -> ((attr -> attrOver) -> (value -> over))
    -- What is reachable here?
    -> Optic attr attrView attrOver
    -> Optic value view over
makeOneToN n superView superOver (Optic sub) =
    Optic
        { view = superView sub.view
        , over = superOver sub.over
        , name = n ++ sub.name
        }



-- Actions


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
view : Getter value view over attr attrOver -> value -> view
view accessor =
    (Optic
        { view = identity
        , over = \_ -> void "`over` should never be called from `get`"
        , name = ""
        }
        |> accessor
        |> internal
    ).view


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> is try
    --> True

    Nothing
        |> is try
    --> False

    ["Stuff", "things"]
        |> is (at 2)
    --> False

    ["Stuff", "things"]
        |> is (at 0)
    --> True

-}
is :
    Getter value (Maybe view) over attr attrOver
    -> value
    -> Bool
is prism sup =
    view prism sup /= Nothing


set :
    -- Setable value view over attrOver
    (Optic attr attr attrOver -> Optic value view over)
    -> attrOver
    -> (value -> over)
set accessor attr =
    (Optic
        { view = \_ -> void "`get` should never be called when `set` is executed"
        , over = always attr
        , name = ""
        }
        |> accessor
        |> internal
    ).over


over :
    (Optic attr attr attrOver -> Optic value view over)
    -> (attr -> attrOver)
    -> value
    -> over
over accessor change =
    (Optic
        { view = identity
        , over = change
        , name = ""
        }
        |> accessor
        |> internal
    ).over


name : (Optic attr attrView attrOver -> Optic value valueView valueOver) -> String
name accessor =
    let
        (Optic relation) =
            accessor
                (Optic
                    { view = \_ -> void "`get` should never be called when `name` is executed"
                    , over = \_ -> void "`over` should never be called when `name` is executed"
                    , name = ""
                    }
                )
    in
    relation.name



-- Helper


void : String -> runTimeError
void s =
    let
        unoptimizedRecursion : String -> runTimeError
        unoptimizedRecursion str =
            unoptimizedRecursion str
    in
    unoptimizedRecursion s
