module Tuple.Accessors exposing (fst, snd)

import Base exposing (Optic)


{-| Lens over the first component of a Tuple

    import Base exposing (view, set, over)
    import Tuple.Accessors as Tuple

    charging : (String, Int)
    charging = ("It's over", 1)

    view Tuple.fst charging
    --> "It's over"

    set Tuple.fst "It's over" charging
    --> ("It's over", 1)

    over Tuple.fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Optic attr view over -> Optic ( attr, ignored ) view ( over, ignored )
fst =
    Base.makeOneToOne "_1" Tuple.first Tuple.mapFirst


{-|

    import Base exposing (view, set, over)
    import Tuple.Accessors as Tuple

    meh : (String, Int)
    meh = ("It's over", 1)

    view Tuple.snd meh
    --> 1

    set Tuple.snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set Tuple.snd 1125
        |> over Tuple.fst (\s -> String.toUpper s ++ "!!!")
        |> over Tuple.snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Optic attr view over -> Optic ( ignored, attr ) view ( ignored, over )
snd =
    Base.makeOneToOne "_2" Tuple.second Tuple.mapSecond
