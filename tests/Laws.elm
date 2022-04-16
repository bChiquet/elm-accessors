module Laws exposing (..)

import Accessors as A exposing (Property, Relation)
import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Maybe exposing (Maybe)
import String
import Test exposing (..)
import Test.Accessors.Record as R


eq : a -> a -> Expectation
eq =
    Expect.equal


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , stuff : List String
    , info : Dict String String
    , things : Array String
    }


suite : Test
suite =
    describe "Laws"
        [ isLens R.name personFuzzer strFun string
        , isLens R.age personFuzzer intFun int
        , isProperty (R.email << A.try) personFuzzer strFun string
        , isProperty (R.stuff << A.at 0) personFuzzer strFun string
        , isProperty (R.stuff << A.each) personFuzzer strFun string
        , isProperty (R.things << A.ix 0) personFuzzer strFun string
        , isProperty (R.things << A.every) personFuzzer strFun string
        , isProperty (R.info << A.key "stuff") personFuzzer maybeStrFun (Fuzz.maybe string)
        , test "Name compositions output `jq` style String's" <|
            \() ->
                A.name (R.info << R.stuff << A.at 7 << R.name)
                    |> eq ".info.stuff(7)?.name"
        ]


type alias Fun a =
    a -> a


strFun : Fuzzer (Fun String)
strFun =
    Fuzz.oneOf
        -- [ Fuzz.map String.reverse string
        -- , String.toUpper
        -- , String.toLower
        [ Fuzz.map String.append string
        , Fuzz.map (\s -> String.append s >> String.reverse) string
        , Fuzz.map (\s -> String.append s >> String.toUpper) string
        , Fuzz.map (\s -> String.append s >> String.toLower) string
        ]


intFun : Fuzzer (Fun Int)
intFun =
    Fuzz.oneOf
        [ Fuzz.map (+) int
        , Fuzz.map (-) int
        , Fuzz.map (*) int
        , Fuzz.map (//) int
        ]


maybeStrFun : Fuzzer (Fun (Maybe String))
maybeStrFun =
    Fuzz.oneOf
        [ Fuzz.map
            (\_ ->
                Maybe.andThen String.toInt
                    >> Maybe.map String.fromInt
            )
            (Fuzz.maybe string)
        ]


personFuzzer : Fuzzer Person
personFuzzer =
    Fuzz.map (\_ -> Person) Fuzz.unit
        |> Fuzz.andMap string
        |> Fuzz.andMap int
        |> Fuzz.andMap (Fuzz.maybe string)
        |> Fuzz.andMap (Fuzz.list string)
        |> Fuzz.andMap (Fuzz.list (Fuzz.tuple ( string, string )) |> Fuzz.map Dict.fromList)
        |> Fuzz.andMap (Fuzz.list string |> Fuzz.map Array.fromList)


isProperty : Property s a wrap -> Fuzzer s -> Fuzzer (Fun a) -> Fuzzer a -> Test
isProperty l fzr fnFzr val =
    describe ("isProperty: " ++ A.name l)
        [ fuzz fzr
            "identity"
            (Expect.true "setter"
                << setter_id l
            )
        , fuzz (Fuzz.tuple3 ( fzr, fnFzr, fnFzr ))
            "composition"
            (\( s, f, g ) ->
                Expect.true "setter" <|
                    setter_composition l s f g
            )
        , fuzz (Fuzz.tuple3 ( fzr, val, val ))
            "set_set"
            (\( s, a, b ) ->
                Expect.true "setter" <|
                    setter_set_set l s a b
            )
        ]


isLens : Property s a a -> Fuzzer s -> Fuzzer (Fun a) -> Fuzzer a -> Test
isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isProperty l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]


setter_id : Property super sub wrap -> super -> Bool
setter_id l s =
    A.over l identity s == s


setter_composition : Property super sub wrap -> super -> Fun sub -> Fun sub -> Bool
setter_composition l s f g =
    A.over l f (A.over l g s) == A.over l (f << g) s


setter_set_set : Property super sub wrap -> super -> sub -> sub -> Bool
setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s


lens_set_get : Property super sub sub -> super -> Bool
lens_set_get l s =
    A.set l (A.get l s) s == s


lens_get_set : Property super sub sub -> super -> sub -> Bool
lens_get_set l s a =
    A.get l (A.set l a s) == a
