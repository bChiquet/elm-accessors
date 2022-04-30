module Accessors exposing
    ( Relation, Accessor, Lens, Lens_, Setable
    , get, set, over, name
    , makeOneToOne, makeOneToN
    , makeOneToOne_, makeOneToN_
    )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.


# Relation

@docs Relation, Accessor, Lens, Lens_, Setable


# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, set, over, name


# Build your own accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

Use for providing meaninful names for you're compositions (re: `name (L.name << L.email) --> ".name.email"`)

@docs makeOneToOne_, makeOneToN_

-}


{-| The most general version of this type that everything else specializes
-}
type alias Accessor dataBefore dataAfter attrBefore attrAfter reachable =
    Relation attrBefore reachable attrAfter -> Relation dataBefore reachable dataAfter


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


type alias Lens_ structure attribute =
    Lens structure attribute attribute attribute



-- type alias Getable structure transformed attribute built reachable =
--     Relation attribute built attribute
--     -> Relation structure reachable transformed


type alias Setable structure transformed attribute built =
    Relation attribute attribute built -> Relation structure attribute transformed


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


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get :
    (Relation attribute built attribute
     -> Relation structure reachable transformed
    )
    -> structure
    -> transformed
get accessor s =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = \super -> super
                    , over = void
                    , name = ""
                    }
                )
    in
    relation.get s


void : a -> b
void super =
    void super


{-| This function gives the name of the function as a string...
-}
name : Accessor a b c d e -> String
name accessor =
    let
        (Relation relation) =
            accessor
                (Relation
                    { get = void
                    , over = void
                    , name = ""
                    }
                )
    in
    relation.name


{-| The set function takes:

  - An accessor,
  - A value of the type `sub`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed to be
    the set value.

```
set (foo << bar) "Hi!" myRecord
```

-}
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
                    { get = void
                    , over = \fn -> fn
                    , name = ""
                    }
                )

        newSuper =
            relation.over (\_ -> value) s
    in
    newSuper



-- type alias Modifiable =
--    Relation attribute x y -> Relation structure a transformed


{-| The over function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
over (foo << qux) ((+) 1) myRecord
```

-}
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
                    { get = void
                    , over = \fn -> fn
                    , name = ""
                    }
                )
    in
    relation.over change s


{-| This function lets you build an accessor for containers that have
a 1:1 relation with what they contain, such as a record and one of its fields:

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne :
    (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> (Relation attribute reachable wrap -> Relation structure reachable wrap)
makeOneToOne =
    makeOneToOne_ ""


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Relation field sub wrap -> Relation { rec | foo : field } sub wrap
    foo =
        makeOneToOne_
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
makeOneToOne_ :
    String
    -> (structure -> attribute)
    -> ((attribute -> attribute) -> structure -> structure)
    -> (Relation attribute reachable wrap -> Relation structure reachable wrap)
makeOneToOne_ n getter mapper (Relation sub) =
    Relation
        { get = \super -> sub.get (getter super)
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }


{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN
            List.map
            List.map

n.b. implementing those is usually considerably simpler than the type suggests.

-}
makeOneToN :
    ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here? And this is obviously not Lens so?
    -> (Relation attribute reachable built -> Relation structure reachable transformed)
makeOneToN =
    makeOneToN_ ""


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
    each =
        makeOneToN_ "[]"
            List.map
            List.map

-}
makeOneToN_ :
    String
    -> ((attribute -> built) -> structure -> transformed)
    -> ((attribute -> attribute) -> structure -> structure)
    -- What is reachable here?
    -> Relation attribute reachable built
    -> Relation structure reachable transformed
makeOneToN_ n getter mapper (Relation sub) =
    Relation
        { get = \super -> getter sub.get super
        , over = \change super -> mapper (sub.over change) super
        , name = n ++ sub.name
        }
