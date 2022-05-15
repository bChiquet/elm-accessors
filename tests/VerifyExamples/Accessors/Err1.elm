module VerifyExamples.Accessors.Err1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Lens as L
import Accessors exposing (..)



maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
maybeRecord = { foo = Ok { bar = 2 }
              , qux = Err "Not an Int"
              }



spec1 : Test.Test
spec1 =
    Test.test "#err: \n\n    over (L.foo << err) String.toUpper maybeRecord\n    --> { foo = Ok { bar = 2 }, qux = Err \"Not an Int\" }" <|
        \() ->
            Expect.equal
                (
                over (L.foo << err) String.toUpper maybeRecord
                )
                (
                { foo = Ok { bar = 2 }, qux = Err "Not an Int" }
                )