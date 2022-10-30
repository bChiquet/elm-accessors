module Tuple.Accessors exposing (fst, snd)

import Base exposing (Optic)


{-| Lens over the first component of a Tuple

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get fst charging
    --> "It's over"

    set fst "It's over" charging
    --> ("It's over", 1)

    over fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Optic sub reachable wrap -> Optic ( sub, x ) reachable wrap
fst =
    Base.makeOneToOne "_1" Tuple.first Tuple.mapFirst


{-|

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get snd meh
    --> 1

    set snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set snd 1125
        |> over fst (\s -> String.toUpper s ++ "!!!")
        |> over snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Optic sub reachable wrap -> Optic ( x, sub ) reachable wrap
snd =
    Base.makeOneToOne "_2" Tuple.second Tuple.mapSecond
