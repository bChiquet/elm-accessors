module Accessors.Internal exposing
  ( Relation(..)
  , id
  )


{-| A `Relation super sub wrap` is a type describing how to interact with a
`sub` data when given a `super` data.

The `wrap` exists because some types can't ensure that `get` will return a
`sub`. For instance, `Maybe sub` may not actually contain a `sub`. Therefore,
`get` returns a `wrap` which, in that example, will be `Maybe sub`

Implementation: A relation is a banal record storing a `get` function and an
`over` function.
-}
type Relation super sub wrap = 
    Relation { get : super -> wrap
             , over : (sub -> sub) -> (super -> super) }


{-| id is a neutral `Relation`. It is used to end a braid of accessors (see
the implementation for get, set and over).
-}
id : Relation a a a
id =
  Relation { get  = \a -> a
           , over = \change -> (\a -> change a)
           }
