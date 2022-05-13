module VerifyExamples.Accessors.Keyed1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Dict exposing (Dict)
import Lens as L
import Accessors exposing (..)



multiplyIfA : (String, { bar : Int }) -> (String, { bar : Int })
multiplyIfA ( key, ({ bar } as rec) ) =
    if key == "a" then
        ( key, { bar = bar * 10 } )
    else
        (key, rec)
dictRecord : {foo : Dict String {bar : Int}}
dictRecord = { foo = [ ("a", { bar = 2 })
                     , ("b", { bar = 3 })
                     , ("c", { bar = 4 })
                     ] |> Dict.fromList
             }



spec1 : Test.Test
spec1 =
    Test.test "#keyed: \n\n    get (L.foo << keyed << snd << L.bar) dictRecord\n    --> [(\"a\", 2), (\"b\", 3), (\"c\", 4)] |> Dict.fromList" <|
        \() ->
            Expect.equal
                (
                get (L.foo << keyed << snd << L.bar) dictRecord
                )
                (
                [("a", 2), ("b", 3), ("c", 4)] |> Dict.fromList
                )