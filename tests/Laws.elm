module Laws exposing (..)

import Accessors as A
import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Lens as L
import Maybe exposing (Maybe)
import String
import Test exposing (..)


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
    describe "Laws Specs"
        [ isLens L.name personFuzzer strFun string
        , isLens L.age personFuzzer intFun int
        , isSetable (L.email << A.try) personFuzzer strFun string

        -- TODO: How to express laws for "Prism"-ish things elm-monocle calls this Optional.
        -- , isOptional (L.email << A.try)
        , isSetable (L.stuff << A.at 0) personFuzzer strFun string
        , isSetable (L.stuff << A.each) personFuzzer strFun string
        , isSetable (L.things << A.ix 0) personFuzzer strFun string
        , isSetable (L.things << A.every) personFuzzer strFun string
        , isLens (L.info << A.key "stuff")
            personFuzzer
            maybeStrFun
            (Fuzz.maybe string)
        , test "Name compositions output `jq` style String's" <|
            \() ->
                A.name (L.info << L.stuff << A.at 7 << L.name)
                    |> eq ".info.stuff(7)?.name"
        ]


type alias Function a =
    a -> a


strFun : Fuzzer (Function String)
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


intFun : Fuzzer (Function Int)
intFun =
    Fuzz.oneOf
        [ Fuzz.map (+) int
        , Fuzz.map (-) int
        , Fuzz.map (*) int
        , Fuzz.map (//) int
        ]


maybeStrFun : Fuzzer (Function (Maybe String))
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



-- isSetable : Setable structure transformed attribute built -> Fuzzer structure -> Fuzzer (Function attribute) -> Fuzzer attribute -> Test


isSetable l fzr fnFzr val =
    describe ("isSetable: " ++ A.name l)
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



-- isLens :
--     Lens value view over
--     -> Fuzzer value
--     -> Fuzzer (Function view)
--     -> Fuzzer view
--     -> Test


isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isSetable l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]



-- setter_id : Setable structure transformed attribute built -> structure -> Bool


setter_id l s =
    A.map l identity s == s



-- setter_composition :
--     Setable structure transformed attribute built
--     -> structure
--     -> Function attribute
--     -> Function attribute
--     -> Bool


setter_composition l s f g =
    A.map l f (A.map l g s) == A.map l (f << g) s



-- setter_set_set :
--     Setable structure transformed attribute built
--     -> structure
--     -> attribute
--     -> attribute
--     -> Bool


setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s



-- lens_set_get : Lens value view over -> value -> Bool


lens_set_get l s =
    A.set l (A.get l s) s == s



-- lens_get_set : Lens value view over -> value -> over -> Bool


lens_get_set l s a =
    A.get l (A.set l a s) == a
