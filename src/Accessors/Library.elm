module Accessors.Library exposing (onEach, try)

{-| This library contains common accessors.

@docs onEach, try
-}

import Accessors exposing (Relation, makeOneToN)

{-| This accessor combinator lets you access values inside lists.

    listRecord = {foo = [ {bar = 2}
                        , {bar = 3}
                        , {bar = 4}
                        ]
                 }

    get (foo << onEach << bar) listRecord
    -- returns [2, 3, 4] 

    over (foo << onEach << bar) ((+) 1) listRecord
    -- returns {foo = [{bar = 3}, {bar = 4}, {bar = 5}] }
-}
onEach : Relation super sub wrap -> Relation (List super) sub (List wrap)
onEach = makeOneToN List.map List.map


{-| This accessor combinator lets you access values inside Maybe.

    maybeRecord = { foo = Just {bar = 2}
                  , qux = Nothing
                  }

    get (foo << try << bar) maybeRecord
    -- returns Just 2

    get (qux << try << bar) maybeRecord
    -- returns Nothing

    over (foo << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 3}, qux = Nothing}

    over (qux << try << bar) ((+) 1) maybeRecord
    -- returns {foo = Just {bar = 2}, qux = Nothing}
-}
try : Relation super sub wrap -> Relation (Maybe super) sub (Maybe wrap)
try = makeOneToN Maybe.map Maybe.map
