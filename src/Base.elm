module Base exposing
    ( Relation, Accessor, Lens, Lens_, Setable
    , makeOneToOne, makeOneToN
    , get, is, name, over, set
    )

{-|


# Relation

@docs Relation, Accessor, Lens, Lens_, Setable


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

-}


type alias Accessor dataBefore dataAfter attrBefore attrAfter reachable =
    Relation attrBefore reachable attrAfter -> Relation dataBefore reachable dataAfter


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.

-}
type Relation structure attribute wrap
    = Relation
        { get : structure -> wrap
        , over : (attribute -> attribute) -> (structure -> structure)
        , name : String
        }


{-| This is an approximation of Van Laarhoven encoded Lenses which enable the
the callers to use regular function composition to build more complex nested
updates of more complicated types.

But the original "Lens" type looked more like:

    type alias Lens structure attribute =
        { get : structure -> attribute
        , set : structure -> attribute -> structure
        }

unfortunately these can't be composed without
defining custom `composeLens`, `composeIso`, `composePrism`, style functions.

whereas with this approach we're able to make use of Elm's built in `<<` operator
to get/set/over deeply nested data.

-}
type alias
    Lens
        -- Structure Before Action
        structure
        -- Structure After Action
        transformed
        -- Focus Before action
        attribute
        -- Focus After action
        built
    =
    Relation attribute built transformed
    -> Relation structure built transformed


{-| Simplified version of Lens but seems to break type inference for more complicated compositions.
-}
type alias Lens_ structure attribute =
    Lens structure attribute attribute attribute


{-| Type of a composition of accessors that `set` can be called with.
-}
type alias Setable structure transformed attribute built =
    Relation attribute attribute built -> Relation structure attribute transformed


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
    -> (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> (Relation attribute reachable wrap -> Relation structure reachable wrap)
makeOneToOne n getter mapper (Relation sub) =
    Relation
        { get = \super -> sub.get (getter super)
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


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
    -> ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here?
    -> Relation attribute reachable built
    -> Relation structure reachable transformed
makeOneToN n getter mapper (Relation sub) =
    Relation
        { get = \super -> getter sub.get super
        , over = \change super -> mapper (sub.over change) super
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
    (Relation attribute built attribute -> Relation structure reachable transformed)
    -> structure
    -> transformed
get accessor s =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = identity
                    , over = \_ -> void "`over` should never be called from `get`"
                    , name = ""
                    }
                )
    in
    relation.get s


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
    (Relation attribute built attribute -> Relation structure reachable (Maybe transformed))
    -> structure
    -> Bool
is prism sup =
    get prism sup /= Nothing


set :
    Setable structure transformed attribute built
    -> attribute
    -> structure
    -> structure
set accessor value s =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = \_ -> void "`get` should never be called when `set` is executed"
                    , over = identity
                    , name = ""
                    }
                )
    in
    relation.over (\_ -> value) s


over :
    (Relation attribute attribute built -> Relation structure attribute transformed)
    -> (attribute -> attribute)
    -> structure
    -> structure
over accessor change s =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = \_ -> void "`get` should never be called when `over` is executed"
                    , over = identity
                    , name = ""
                    }
                )
    in
    relation.over change s


name : Accessor a b c d e -> String
name accessor =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = \_ -> void "`get` should never be called when `name` is executed"
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
