module Accessors.Library exposing (onEach, try, dictEntry)

{-| This library contains common accessors.

@docs onEach, try, dictEntry

-}

import Accessors as A exposing (Relation)
import Dict exposing (Dict)


{-| Re-Exported from Accessors for backwards compatibility
-}
onEach : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
onEach =
    A.each


{-| Re-Exported from Accessors for backwards compatibility
-}
try : Relation attribute built transformed -> Relation (Maybe attribute) built (Maybe transformed)
try =
    A.try


{-| Re-Exported from Accessors for backwards compatibility
-}
dictEntry : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
dictEntry =
    A.key
