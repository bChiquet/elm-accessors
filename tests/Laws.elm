module Laws exposing (..)

import Accessors as A exposing (Iso, Lens, Optic, Prism, SimpleIso, SimpleLens, SimpleOptic, SimplePrism, Traversal, Y, new, try)
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
    describe "Suite!"
        [ test "Name compositions output `jq` style String's" <|
            \() ->
                A.name (L.info << L.stuff << A.at 7 << L.name)
                    |> eq ".info.stuff[7]?.name"
        , describe "Laws Specs"
            [ isSetter (L.email << A.just_) personFuzzer strFun string
            , isSetter (L.stuff << A.at 0) personFuzzer strFun string
            , isSetter (L.stuff << A.each) personFuzzer strFun string
            , isSetter (L.things << A.ix 0) personFuzzer strFun string
            , isSetter (L.things << A.every) personFuzzer strFun string
            , isLens L.name personFuzzer strFun string
            , isLens L.age personFuzzer intFun int
            , isLens (L.info << A.key "stuff") personFuzzer maybeStrFun (Fuzz.maybe string)
            , isPrism A.just_ (Fuzz.maybe string) string
            , isPrism A.ok_ (Fuzz.result int string) string
            , isPrism A.err_ (Fuzz.result int string) int
            ]
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


isSetter : (Optic pr ls c c c c -> Optic pr ls a a c c) -> Fuzzer a -> Fuzzer (c -> c) -> Fuzzer c -> Test
isSetter l fzr fnFzr val =
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


isPrism : (Optic () ls a a a a -> Optic () ls s s a a) -> Fuzzer s -> Fuzzer a -> Test
isPrism pr fzrS fzrA =
    describe ("isPrism: " ++ A.name pr)
        [ fuzz (Fuzz.tuple ( fzrS, fzrA ))
            "yin"
            (\( s, a ) ->
                Expect.true "yin & yang"
                    (prism_yin pr a && prism_yang pr s)
            )

        -- , isTraversal
        -- , fuzz (Fuzz.maybe string) "just_ is a prism_yin" <|
        --     \maybeStr ->
        --         Expect.true "prism_yin" <|
        --             prism_yin A.just_ maybeStr
        -- , fuzz (Fuzz.maybe string) "just_ is a prism_yang" <|
        --     \maybeStr ->
        --         Expect.true "prism_yang" <|
        --             prism_yang A.just_ maybeStr
        -- , fuzz (Fuzz.result int string) "ok_ is a prism_yin" <|
        --     \resIntStr ->
        --         Expect.true "prism_yin" <|
        --             prism_yin A.ok_ resIntStr
        -- , fuzz (Fuzz.result int string) "ok_ is a prism_yang" <|
        --     \resIntStr ->
        --         Expect.true "prism_yang" <|
        --             prism_yang A.ok_ resIntStr
        -- , fuzz (Fuzz.result string int) "err_ is a prism_yin" <|
        --     \resIntStr ->
        --         Expect.true "prism_yin" <|
        --             prism_yin A.err_ resIntStr
        -- , fuzz (Fuzz.result int string) "err_ is a prism_yang" <|
        --     \resIntStr ->
        --         Expect.true "prism_yang" <|
        --             prism_yang A.err_ resIntStr
        ]


isLens : (Optic pr Y b b b b -> Optic pr Y a a b b) -> Fuzzer a -> Fuzzer (b -> b) -> Fuzzer b -> Test
isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isSetter l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]


setter_id : (Optic pr ls a a a a -> Optic pr ls b b a a) -> b -> Bool
setter_id l s =
    A.map l identity s == s


setter_composition : (Optic pr ls b b b b -> Optic pr ls t t b b) -> t -> (b -> b) -> (b -> b) -> Bool
setter_composition l s f g =
    A.map l f (A.map l g s) == A.map l (f << g) s


setter_set_set : (Optic pr ls a d a d -> Optic pr ls t t a d) -> t -> d -> d -> Bool
setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s


lens_set_get : (Optic pr Y a a a a -> Optic pr Y b b a a) -> b -> Bool
lens_set_get l s =
    A.set l (A.get l s) s == s


lens_get_set : (Optic pr Y c c c c -> Optic pr Y t t c c) -> t -> c -> Bool
lens_get_set l s a =
    A.get l (A.set l a s) == a


prism_yin : (Optic () ls a a a a -> Optic () ls s s a a) -> a -> Bool
prism_yin l a =
    try l (new l a) == Just a


prism_yang : (Optic () ls a a a a -> Optic () ls s s a a) -> s -> Bool
prism_yang l s =
    (Maybe.withDefault s <| Maybe.map (new l) (try l s)) == s



-- iso_hither : SimpleIso pr ls s a -> s -> Bool
-- iso_hither l s = s ^. cloneIso l . from l == s
-- iso_yon : SimpleIso pr ls s a -> a -> Bool
-- iso_yon l a = a ^. from l . cloneIso l == a
-- traverse_pure : LensLike' f s a -> s -> Bool
-- traverse_pure l s = l pure s == (pure s : f s)
-- traverse_pureMaybe : Eq s => LensLike' Maybe s a -> s -> Bool
-- traverse_pureMaybe = traverse_pure
-- traverse_pureList : Eq s => LensLike' [] s a -> s -> Bool
-- traverse_pureList = traverse_pure
-- traverse_compose : (Applicative f, Applicative g, Eq (f (g s)))
--                     => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
-- traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s
