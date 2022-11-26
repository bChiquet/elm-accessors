module Json.Accessors exposing
    ( at
    , bool_
    , float_
    , int_
    , key
    , list_
    , null_
    , object_
    , string_
    , value_
    )

import Base exposing (..)
import Dict exposing (Dict)
import Dict.Accessors as Dict
import Generic
import Generic.Json as Json
import Json.Encode as Encode
import List.Accessors as List


value_ : Optic pr ls Generic.Value Generic.Value x y -> Prism pr String String x y
value_ =
    prism "JSON"
        (Json.encode >> Encode.encode 0)
        (\v -> Json.decode v |> Result.mapError (\_ -> v))


int_ : Optic pr ls Int Int x y -> Prism pr String String x y
int_ =
    value_
        << prism "int"
            Generic.Int
            (Generic.toInt
                >> Maybe.map Ok
                >> Maybe.withDefault (Err Generic.Null)
            )


float_ : Optic pr ls Float Float x y -> Prism pr String String x y
float_ =
    value_
        << prism "float"
            Generic.Float
            (Generic.toFloat
                >> Maybe.map Ok
                >> Maybe.withDefault (Err Generic.Null)
            )


string_ : Optic pr ls String String x y -> Prism pr String String x y
string_ =
    value_
        << prism "string"
            Generic.String
            (\v ->
                case v of
                    Generic.String s ->
                        Ok s

                    _ ->
                        Err v
            )


bool_ : Optic pr ls Bool Bool x y -> Prism pr String String x y
bool_ =
    value_
        << prism "bool"
            Generic.Bool
            (\v ->
                case v of
                    Generic.Bool b ->
                        Ok b

                    _ ->
                        Err v
            )


null_ : Optic pr ls () b x y -> Prism pr String String x y
null_ =
    value_
        << prism "null"
            (always Generic.Null)
            (\v ->
                case v of
                    Generic.Null ->
                        Ok ()

                    _ ->
                        Err v
            )


list_ : Optic pr ls (List Generic.Value) (List Generic.Value) x y -> Prism pr String String x y
list_ =
    value_
        << prism "List"
            Generic.List
            (\v ->
                case v of
                    Generic.List a ->
                        Ok a

                    _ ->
                        Err v
            )


object_ : Optic pr ls (Dict String Generic.Value) (Dict String Generic.Value) x y -> Prism pr String String x y
object_ =
    value_
        << prism "Dict"
            (Dict.toList >> (List.map << Tuple.mapFirst) Generic.String >> Generic.dictFromList)
            (\v ->
                case Generic.toDict v of
                    Just o ->
                        Ok o

                    Nothing ->
                        Err v
            )


key : String -> Optic pr ls (Maybe Generic.Value) (Maybe Generic.Value) x y -> Traversal String String x y
key s =
    object_ << Dict.at s


at : Int -> Optic pr ls Generic.Value Generic.Value x y -> Traversal String String x y
at i =
    list_ << List.at i
