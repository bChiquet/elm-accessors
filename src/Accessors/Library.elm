module Accessors.Library exposing (dictEntry, onEach, try)

import Accessors as A exposing (Relation)
import Dict exposing (Dict)


onEach : Relation attribute built transformed -> Relation (List attribute) built (List transformed)
onEach =
    A.each


try : Relation sub path wrap -> Relation (Maybe sub) path (Maybe wrap)
try =
    A.try


dictEntry : comparable -> Relation (Maybe v) reachable wrap -> Relation (Dict comparable v) reachable wrap
dictEntry =
    A.key
