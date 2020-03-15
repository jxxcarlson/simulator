module Value exposing
    ( create
    , gt
    , gte
    , intValue
    , mul
    , toFloat_
    )

import Internal.Types exposing (Cents(..), Currency, Value(..))
import Internal.Value


create : Currency -> Float -> Value
create =
    Internal.Value.create


mul : Int -> Value -> Value
mul =
    Internal.Value.mul


gte : Value -> Value -> Maybe Bool
gte =
    Internal.Value.gte


gt : Value -> Value -> Maybe Bool
gt =
    Internal.Value.gt


intValue : Value -> Int
intValue (Value currency cents) =
    intValueOfCents cents


toFloat_ : Value -> Float
toFloat_ value =
    value
        |> intValue
        |> toFloat
        |> (\x -> x / 100)


intValueOfCents : Cents -> Int
intValueOfCents (Cents k) =
    k
