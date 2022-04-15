module Accessors exposing
  ( Relation
  , get, set, over
  , makeOneToOne, makeOneToN
  )

{-| Relations are interfaces to document the relation between two data
structures. For convenience, we'll call the containing structure `super`, and
the contained structure `sub`. What a `Relation` claims is that a `super` is
referencing a `sub` in some way.

Relations are the building blocks of accessors. An accessor is a function that
expects a `Relation` and builds a new relation with it. Accessors are
composable, which means you can build a chain of relations to manipulate nested
structures without handling the packing and the unpacking.

# Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor. 

@docs get, set, over

# Build accessors

Accessors are built using these functions:

@docs makeOneToOne, makeOneToN

# Relation

@docs Relation
-}

import Accessors.Internal as Internal exposing (Relation(..), id)

{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.
-}
type alias Relation super sub wrap = Internal.Relation super sub wrap


{-| The get function takes:
* An accessor,
* A datastructure with type `super`
and returns the value accessed by that combinator.
```
get (foo << bar) myRecord 
```
-}
get : (Relation sub sub sub -> Relation super sub wrap) -> super -> wrap
get accessor s = 
  let (Relation relation) = (accessor id)
  in relation.get s


{-|The set function takes:
* An accessor, 
* A value of the type `sub`,
* A datastructure with type `super`
and it returns the data structure, with the accessible field changed to be
the set value.
```
set (foo << bar) "Hi!" myRecord
```
-}
set : (Relation sub sub sub -> Relation super sub wrap) -> sub -> super -> super
set accessor value s = 
  let (Relation relation) = (accessor id)
  in relation.over (\_ -> value) s


{-|The over function takes:
* An accessor, 
* A function `(sub -> sub)`,
* A datastructure with type `super`
and it returns the data structure, with the accessible field changed by applying
the function to the existing value.
```
over (foo << qux) ((+) 1) myRecord
```
-}
over : (Relation sub sub sub -> Relation super sub wrap)
    -> (sub -> sub)
    -> super
    -> super
over accessor change s = 
  let (Relation relation) = (accessor id)
  in relation.over change s


{-| This function lets you build an accessor for containers that have
a 1:1 relation with what they contain, such as a record and one of its fields:

```
foo : Relation field sub wrap -> Relation {rec | foo : field} sub wrap
foo =
  makeOneToOne
    .foo
    (\change rec -> {rec | foo = change rec.foo })
```
-}
makeOneToOne :  (super -> sub)
             -> ((sub -> sub) -> super -> super)
             -> Relation sub   reachable wrap
             -> Relation super reachable wrap
makeOneToOne getter mapper (Relation sub) =
  Relation { get  = \super -> sub.get (getter super)
           , over = \change super -> mapper (sub.over change) super
           }

{-| This function lets you build an accessor for containers that have
a 1:N relation with what they contain, such as `List` (0-N cardinality) or
`Maybe` (0-1). E.g.:
```
onEach : Relation elem sub wrap -> Relation (List elem) sub (List wrap)
onEach =
  makeOneToN
    List.map
    List.map
```
n.b. implementing those is usually considerably simpler than the type suggests.
-}
makeOneToN :  ((sub -> subWrap) -> super -> superWrap)
           -> ((sub -> sub) -> super -> super)
           -> Relation sub   reachable subWrap
           -> Relation super reachable superWrap
makeOneToN getter mapper (Relation sub) =
  Relation { get  = \super -> getter sub.get super
           , over = \change super -> mapper (sub.over change) super
           }
