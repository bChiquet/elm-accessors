module Test.Accessors.Record exposing (r)

import Accessors exposing (makeOneToOne)

r = { bar = makeOneToOne
              .bar
              (\change rec -> {rec | bar = change rec.bar})
    , foo = makeOneToOne
              .foo
              (\change rec -> {rec | foo = change rec.foo})
    , qux = makeOneToOne
              .qux
              (\change rec -> {rec | qux = change rec.qux})
    }
