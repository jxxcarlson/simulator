module Internal.Value exposing (..)


import Internal.Types exposing(Value(..), Cents(..), Currency(..))
import Internal.Cents as Cents

map : (Cents -> Cents) -> Value -> Value
map f (Value curr cents) =
    (Value curr (f cents))


{-|

    import Internal.Types exposing(..)
    import Internal.Money as Money

    v : Value
    v = Value Money.usDollars (Cents 200)

    imap (\k (Cents c) -> (Cents (k * c))) 2 v
    --> Value Money.usDollars (Cents 400)

-}
imap : (Int -> Cents -> Cents) -> Int -> Value -> Value
imap f k (Value curr c) =
    Value curr (f k c)

map2 : (Cents -> Cents -> Cents ) -> Value -> Value -> Maybe Value
map2 f (Value curr1 cents1)  (Value curr2 cents2)=
    case curr1 == curr2 of
        True ->  Just (Value curr1 (f cents1 cents2))
        False -> Nothing

map2E : (Cents -> Cents -> a ) -> Value -> Value -> Maybe a
map2E f (Value curr1 cents1)  (Value curr2 cents2)=
    case curr1 == curr2 of
        True ->  Just (f cents1 cents2)
        False -> Nothing
{-|
    import Internal.Types exposing(Value(..), Cents(..))
    import Internal.Money as Money

    v1 : Value
    v1 = Value Money.usDollars (Cents 200)

    v2 : Value
    v2 = Value Money.usDollars (Cents 100)

    v3 : Value
    v3 = Value Money.greenBucks (Cents 100)

    add v1 v2
    --> Just <| Value Money.usDollars (Cents 300)

    add v1 v3
    --> Nothing


-}
add : Value -> Value -> Maybe Value
add a b =
    map2 Cents.add a b

{-|
    import Internal.Types exposing(Value(..), Cents(..))
    import Internal.Money as Money

    v1 : Value
    v1 = Value Money.usDollars (Cents 200)

    v2 : Value
    v2 = Value Money.usDollars (Cents 100)

    v3 : Value
    v3 = Value Money.greenBucks (Cents 100)

    subtract v1 v2
    -- Just <| Value (Cents 100)

    subtract v1 v3
    --> Nothing


-}
subtract : Value -> Value -> Maybe Value
subtract a b =
    map2 Cents.subtract a b


{-|

    import Internal.Types exposing(Value(..), Cents(..))
    import Internal.Money as Money

    v : Value
    v = Value Money.usDollars (Cents 200)

    mul 3 v
    --> Value Money.usDollars (Cents 600)

-}
mul : Int -> Value -> Value
mul k v =
    imap Cents.mul k v


{-|
    import Internal.Types exposing(Value(..), Cents(..))
    import Internal.Money as Money

    v1 : Value
    v1 = Value Money.usDollars (Cents 200)

    v2 : Value
    v2 = Value Money.usDollars (Cents 100)

    v3 : Value
    v3 = Value Money.greenBucks (Cents 100)

    gt v1 v2
    -- Just Tue

    gt v1 v1
    -- Just False

    gt v2 v1
    -- Just False

    gt v1 v3
    --> Nothing


-}
gt : Value -> Value -> Maybe Bool
gt a b =
    map2E Cents.gte a b


{-|
    import Internal.Types exposing(Value(..), Cents(..))
    import Internal.Money as Money

    v1 : Value
    v1 = Value Money.usDollars (Cents 200)

    v2 : Value
    v2 = Value Money.usDollars (Cents 100)

    v3 : Value
    v3 = Value Money.greenBucks (Cents 100)

    gte v1 v2
    -- Just Tue

    gte v1 v1
    -- Just True

    gte v2 v1
    -- Just False

    gte v1 v3
    --> Nothing


-}
gte : Value -> Value -> Maybe Bool
gte a b =
    map2E Cents.gt a b

create : Currency -> Float -> Value
create currency_ amount_ =
    Value currency_ (Cents (round (amount_/100.0)))


