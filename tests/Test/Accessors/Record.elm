module Test.Accessors.Record exposing (r)

import Accessors exposing (Accessor(..))

r = {
    bar = \(Accessor sub) ->
      Accessor { get  = \super -> sub.get super.bar
               , over = \f -> \super -> { super | bar = sub.over f super.bar }
               }
    ,
    foo = \(Accessor sub) ->
      Accessor { get  = \super -> sub.get super.foo
               , over = \f -> \super -> { super | foo = sub.over f super.foo }
               }
    ,
    qux = \(Accessor sub) ->
      Accessor { get  = \super -> sub.get super.qux
               , over = \f -> \super -> { super | qux = sub.over f super.qux }
               }
    }
