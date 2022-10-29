module SpecLazy exposing (suite)

import Accessors exposing (each, key, makeOneToN, makeOneToOne, try)
import Accessors.Lazy exposing (get, over, set)
import Dict exposing (Dict)
import Expect
import Lens as L
import Test exposing (Test, describe, test)


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
                    Expect.equal
                        (get L.foo simpleRecord)
                        3
            , test "nested get" <|
                \_ ->
                    Expect.equal
                        (get (L.foo << L.bar) nestedRecord)
                        "Yop"
            , test "get in list" <|
                \_ ->
                    Expect.equal
                        (get (L.bar << each << L.foo) recordWithList)
                        [ 3, 5 ]
            , test "get in Just" <|
                \_ ->
                    Expect.equal
                        (get (L.bar << try << L.qux) maybeRecord)
                        (Just False)
            , test "get in Nothing" <|
                \_ ->
                    Expect.equal
                        (get (L.foo << try << L.bar) maybeRecord)
                        Nothing
            , describe "dict"
                [ test "get present" <|
                    \_ ->
                        Expect.equal
                            (get (key "foo") dict)
                            (Just 7)
                , test "get absent" <|
                    \_ ->
                        Expect.equal
                            (get (key "bar") dict)
                            Nothing
                , test "nested get present" <|
                    \_ ->
                        Expect.equal
                            (get (L.bar << key "foo") recordWithDict)
                            (Just 7)
                , test "nested get absent" <|
                    \_ ->
                        Expect.equal
                            (get (L.bar << key "bar") recordWithDict)
                            Nothing
                , test "get with try" <|
                    \_ ->
                        Expect.equal
                            (get (key "foo" << try << L.bar) dictWithRecord)
                            (Just "Yop")
                ]
            ]
        , describe "set"
            [ test "simple set" <|
                \_ ->
                    let
                        updatedExample =
                            set L.qux True simpleRecord
                    in
                    Expect.equal
                        updatedExample.qux
                        True
            , test "nested set" <|
                \_ ->
                    let
                        updatedExample =
                            set (L.foo << L.foo) 5 nestedRecord
                    in
                    Expect.equal
                        updatedExample.foo.foo
                        5
            , test "set in list" <|
                \_ ->
                    let
                        updatedExample =
                            set (L.bar << each << L.bar) "Why, hello" recordWithList
                    in
                    Expect.equal
                        (get (L.bar << each << L.bar) updatedExample)
                        [ "Why, hello", "Why, hello" ]
            , test "set in Just" <|
                \_ ->
                    let
                        updatedExample =
                            set (L.bar << try << L.foo) 4 maybeRecord
                    in
                    Expect.equal
                        (get (L.bar << try << L.foo) updatedExample)
                        (Just 4)
            , test "set in Nothing" <|
                \_ ->
                    let
                        updatedExample =
                            set (L.foo << try << L.bar) "Nope" maybeRecord
                    in
                    Expect.equal
                        (get (L.foo << try << L.bar) updatedExample)
                        Nothing
            , describe "dict"
                [ test "set currently present to present" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "foo") (Just 9) dict
                        in
                        Expect.equal (get (key "foo") updatedDict) (Just 9)
                , test "set currently absent to present" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "bar") (Just 9) dict
                        in
                        Expect.equal (get (key "bar") updatedDict) (Just 9)
                , test "set currently present to absent" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "foo") Nothing dict
                        in
                        Expect.equal (get (key "foo") updatedDict) Nothing
                , test "set currently absent to absent" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "bar") Nothing dict
                        in
                        Expect.equal (get (key "bar") updatedDict) Nothing
                , test "set with try present" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "foo" << try << L.bar) "Sup" dictWithRecord
                        in
                        Expect.equal (get (key "foo" << try << L.bar) updatedDict) (Just "Sup")
                , test "set with try absent" <|
                    \_ ->
                        let
                            updatedDict =
                                set (key "bar" << try << L.bar) "Sup" dictWithRecord
                        in
                        Expect.equal (get (key "bar" << try << L.bar) updatedDict) Nothing
                ]
            ]
        , describe "over"
            [ test "simple over" <|
                \_ ->
                    let
                        updatedExample =
                            over L.bar (\w -> w ++ " lait") simpleRecord
                    in
                    Expect.equal
                        updatedExample.bar
                        "Yop lait"
            , test "nested over" <|
                \_ ->
                    let
                        updatedExample =
                            over (L.foo << L.qux) (\w -> not w) nestedRecord
                    in
                    Expect.equal
                        updatedExample.foo.qux
                        True
            , test "over list" <|
                \_ ->
                    let
                        updatedExample =
                            over (L.bar << each << L.foo) (\n -> n - 2) recordWithList
                    in
                    Expect.equal
                        (get (L.bar << each << L.foo) updatedExample)
                        [ 1, 3 ]
            , test "over through Just" <|
                \_ ->
                    let
                        updatedExample =
                            over (L.bar << try << L.foo) (\n -> n + 3) maybeRecord
                    in
                    Expect.equal
                        (get (L.bar << try << L.foo) updatedExample)
                        (Just 6)
            , test "over through Nothing" <|
                \_ ->
                    let
                        updatedExample =
                            over (L.foo << try << L.bar) (\w -> w ++ "!") maybeRecord
                    in
                    Expect.equal
                        (get (L.foo << try << L.bar) updatedExample)
                        Nothing
            ]
        , describe "making accessors"
            [ let
                myFoo =
                    makeOneToOne ".foo"
                        .foo
                        (\f rec -> { rec | foo = f rec.foo })
              in
              describe "makeOneToOne"
                [ test "get" <|
                    \_ ->
                        Expect.equal
                            (get (myFoo << L.bar) nestedRecord)
                            "Yop"
                , test "set" <|
                    \_ ->
                        let
                            updatedRec =
                                set (L.foo << myFoo) 1 nestedRecord
                        in
                        Expect.equal updatedRec.foo.foo 1
                , test "over" <|
                    \_ ->
                        let
                            updatedRec =
                                over (myFoo << myFoo) (\n -> n + 3) nestedRecord
                        in
                        Expect.equal updatedRec.foo.foo 6
                ]
            , let
                myOnEach =
                    makeOneToN "[]" List.map List.map
              in
              describe "makeOneToN"
                [ test "get" <|
                    \_ ->
                        Expect.equal
                            (get (L.bar << myOnEach << L.foo) recordWithList)
                            [ 3, 5 ]
                , test "set" <|
                    \_ ->
                        let
                            updatedExample =
                                set (L.bar << myOnEach << L.bar) "Greetings" recordWithList
                        in
                        Expect.equal
                            (get (L.bar << each << L.bar) updatedExample)
                            [ "Greetings", "Greetings" ]
                , test "over" <|
                    \_ ->
                        let
                            updatedExample =
                                over (L.bar << myOnEach << L.foo) (\n -> n - 2) recordWithList
                        in
                        Expect.equal
                            (get (L.bar << each << L.foo) updatedExample)
                            [ 1, 3 ]
                ]
            ]
        ]
