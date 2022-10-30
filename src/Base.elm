module Base exposing
    ( Optic
    , makeOneToOne, makeOneToN
    , get, has, map, name, set
    )

{-|


# Optics

@docs Optic


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

-}


{-| A `Optic value view over` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Optic value view over
    = Optic (Internal value view over)


type alias Internal value view over =
    { view : value -> view
    , over : value -> over
    , name : String
    }


internal : Optic structure view over -> Internal structure view over
internal (Optic i) =
    i



-- type alias Lens value attr view =
--     Optic attr view attr -> Optic value view value


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Optic attr attrView attrOver -> Optic { rec | foo : attr } view over
    foo =
        makeOneToOne
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    String
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

    each : Optic attr view over -> Optic (List attr) view (List over)
    each =
        makeOneToN "[]"
            List.map
            List.map

-}
makeOneToN :
    String
    -> ((attr -> attrView) -> (value -> view))
    -> ((attr -> attrOver) -> (value -> over))
    -> Optic attr attrView attrOver
    -> Optic value view over
makeOneToN n viewAttr overAttr (Optic sub) =
    Optic
        { view = viewAttr sub.view
        , over = overAttr sub.over
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
get :
    (Optic attr attr attrOver -> Optic value view over)
    -> value
    -> view
get accessor =
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
    (Optic attr attr attrOver -> Optic value (Maybe view) over)
    -> value
    -> Bool
has prism sup =
    get prism sup /= Nothing


set :
    (Optic attr attrView attrOver -> Optic value view over)
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


map :
    (Optic attr attrView attrOver -> Optic value view over)
    -> (attr -> attrOver)
    -> value
    -> over
map accessor change =
    (Optic
        { view = \_ -> void "`get` should never be called when `over` is executed"
        , over = change
        , name = ""
        }
        |> accessor
        |> internal
    ).over


name : (Optic attr attrView attrOver -> Optic value valueView valueOver) -> String
name accessor =
    (Optic
        { view = \_ -> void "`get` should never be called when `name` is executed"
        , over = \_ -> void "`over` should never be called when `name` is executed"
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
