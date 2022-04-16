module Spec exposing (suite)

import Accessors exposing (each, get, key, makeOneToN, makeOneToOne, over, set, try)
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)
import Test.Accessors.Record as R


simpleRecord : { foo : number, bar : String, qux : Bool }
simpleRecord =
    { foo = 3, bar = "Yop", qux = False }


anotherRecord : { foo : number, bar : String, qux : Bool }
anotherRecord =
    { foo = 5, bar = "Sup", qux = True }


nestedRecord : { foo : { foo : number, bar : String, qux : Bool } }
nestedRecord =
    { foo = simpleRecord }


recordWithList : { bar : List { foo : number, bar : String, qux : Bool } }
recordWithList =
    { bar = [ simpleRecord, anotherRecord ] }


maybeRecord : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
maybeRecord =
    { bar = Just simpleRecord, foo = Nothing }


dict : Dict String number
dict =
    Dict.fromList [ ( "foo", 7 ) ]


recordWithDict : { bar : Dict String number }
recordWithDict =
    { bar = dict }


dictWithRecord : Dict String { bar : String }
dictWithRecord =
    Dict.fromList [ ( "foo", { bar = "Yop" } ) ]


suite : Test
suite =
    describe "lazy lenses"
        [ describe "get"
            [ test "simple get" <|
                \_ ->
                    get R.foo simpleRecord
                        |> Expect.equal 3
            , test "nested get" <|
                \_ ->
                    get (R.foo << R.bar) nestedRecord
                        |> Expect.equal "Yop"
            , test "get in list" <|
                \_ ->
                    get (R.bar << each << R.foo) recordWithList
                        |> Expect.equal [ 3, 5 ]
            , test "get in Just" <|
                \_ ->
                    get (R.bar << try << R.qux) maybeRecord
                        |> Expect.equal (Just False)
            , test "get in Nothing" <|
                \_ ->
                    get (R.foo << try << R.bar) maybeRecord
                        |> Expect.equal Nothing
            , describe "dict"
                [ test "get present" <|
                    \_ ->
                        get (key "foo") dict
                            |> Expect.equal (Just 7)
                , test "get absent" <|
                    \_ ->
                        get (key "bar") dict
                            |> Expect.equal Nothing
                , test "nested get present" <|
                    \_ ->
                        get (R.bar << key "foo") recordWithDict
                            |> Expect.equal (Just 7)
                , test "nested get absent" <|
                    \_ ->
                        get (R.bar << key "bar") recordWithDict
                            |> Expect.equal Nothing
                , test "get with try" <|
                    \_ ->
                        get (key "foo" << try << R.bar) dictWithRecord
                            |> Expect.equal (Just "Yop")
                ]
            ]
        , describe "set"
            [ test "simple set" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            set R.qux True simpleRecord
                    in
                    updatedExample.qux
                        |> Expect.equal True
            , test "nested set" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            set (R.foo << R.foo) 5 nestedRecord
                    in
                    updatedExample.foo.foo
                        |> Expect.equal 5
            , test "set in list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            set (R.bar << each << R.bar) "Why, hello" recordWithList
                    in
                    get (R.bar << each << R.bar) updatedExample
                        |> Expect.equal [ "Why, hello", "Why, hello" ]
            , test "set in Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            set (R.bar << try << R.foo) 4 maybeRecord
                    in
                    get (R.bar << try << R.foo) updatedExample
                        |> Expect.equal (Just 4)
            , test "set in Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            set (R.foo << try << R.bar) "Nope" maybeRecord
                    in
                    get (R.foo << try << R.bar) updatedExample
                        |> Expect.equal Nothing
            , describe "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "foo") (Just 9) dict
                        in
                        get (key "foo") updatedDict |> Expect.equal (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "bar") (Just 9) dict
                        in
                        get (key "bar") updatedDict |> Expect.equal (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "foo") Nothing dict
                        in
                        get (key "foo") updatedDict |> Expect.equal Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String number
                            updatedDict =
                                set (key "bar") Nothing dict
                        in
                        get (key "bar") updatedDict |> Expect.equal Nothing
                , test "set with try present" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                set (key "foo" << try << R.bar) "Sup" dictWithRecord
                        in
                        get (key "foo" << try << R.bar) updatedDict |> Expect.equal (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        let
                            updatedDict : Dict String { bar : String }
                            updatedDict =
                                set (key "bar" << try << R.bar) "Sup" dictWithRecord
                        in
                        get (key "bar" << try << R.bar) updatedDict |> Expect.equal Nothing
                ]
            ]
        , describe "over"
            [ test "simple over" <|
                \_ ->
                    let
                        updatedExample : { foo : number, bar : String, qux : Bool }
                        updatedExample =
                            over R.bar (\w -> w ++ " lait") simpleRecord
                    in
                    updatedExample.bar
                        |> Expect.equal "Yop lait"
            , test "nested over" <|
                \_ ->
                    let
                        updatedExample : { foo : { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            over (R.foo << R.qux) (\w -> not w) nestedRecord
                    in
                    updatedExample.foo.qux
                        |> Expect.equal True
            , test "over list" <|
                \_ ->
                    let
                        updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                        updatedExample =
                            over (R.bar << each << R.foo) (\n -> n - 2) recordWithList
                    in
                    get (R.bar << each << R.foo) updatedExample
                        |> Expect.equal [ 1, 3 ]
            , test "over through Just" <|
                \_ ->
                    let
                        updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            over (R.bar << try << R.foo) (\n -> n + 3) maybeRecord
                    in
                    get (R.bar << try << R.foo) updatedExample
                        |> Expect.equal (Just 6)
            , test "over through Nothing" <|
                \_ ->
                    let
                        -- updatedExample : { bar : Maybe { foo : number, bar : String, qux : Bool }, foo : Maybe a }
                        updatedExample =
                            over (R.foo << try << R.bar) (\w -> w ++ "!") maybeRecord
                    in
                    get (R.foo << try << R.bar) updatedExample
                        |> Expect.equal Nothing
            ]
        , describe "making accessors"
            [ let
                myFoo =
                    makeOneToOne .foo (\f rec -> { rec | foo = f rec.foo })
              in
              describe "makeOneToOne"
                [ test "get" <|
                    \_ ->
                        get (myFoo << R.bar) nestedRecord
                            |> Expect.equal "Yop"
                , test "set" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                set (R.foo << myFoo) 1 nestedRecord
                        in
                        updatedRec.foo.foo |> Expect.equal 1
                , test "over" <|
                    \_ ->
                        let
                            updatedRec : { foo : { foo : number, bar : String, qux : Bool } }
                            updatedRec =
                                over (myFoo << myFoo) (\n -> n + 3) nestedRecord
                        in
                        updatedRec.foo.foo |> Expect.equal 6
                ]
            , let
                myOnEach =
                    makeOneToN List.map List.map
              in
              describe "makeOneToN"
                [ test "get" <|
                    \_ ->
                        get (R.bar << myOnEach << R.foo) recordWithList
                            |> Expect.equal [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                set (R.bar << myOnEach << R.bar) "Greetings" recordWithList
                        in
                        get (R.bar << each << R.bar) updatedExample
                            |> Expect.equal [ "Greetings", "Greetings" ]
                , test "over" <|
                    \_ ->
                        let
                            updatedExample : { bar : List { foo : number, bar : String, qux : Bool } }
                            updatedExample =
                                over (R.bar << myOnEach << R.foo) (\n -> n - 2) recordWithList
                        in
                        get (R.bar << each << R.foo) updatedExample
                            |> Expect.equal [ 1, 3 ]
                ]
            ]
        ]
