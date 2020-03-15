module Internal.Cents exposing (..)

import Internal.Types exposing(Cents(..))


map : (Int -> Int) -> Cents -> Cents
map f (Cents k) =
    Cents (f k)

{-|

    import Internal.Types exposing(Cents(..))

    imap (\k c -> k * c) 2 (Cents 3)
    --> Cents 6
-}
imap : (Int -> Int -> Int) -> Int -> Cents -> Cents
imap f k (Cents c) =
   Cents (f k c)

map2 : (Int -> Int -> Int) -> Cents -> Cents -> Cents
map2 f (Cents m) (Cents n) =
    Cents (f m n)

map2E : (Int -> Int -> a) -> Cents -> Cents -> a
map2E f (Cents m) (Cents n) =
   f m n
{-|

    import Internal.Types exposing(Cents(..))

  addCents 2 3
  --> Cents 5


-}
add : Cents -> Cents -> Cents
add a b =
    map2 (+) a b


mul : Int -> Cents -> Cents
mul k c =
    imap (\k_ n_ -> k_ * n_) k c

{-|

    import Internal.Types exposing(Cents(..))

    negateCents (Cents 3)
    --> Cents -3

-}
negate : Cents -> Cents
negate cents =
    map (\k -> -k) cents

{-|
    import Internal.Types exposing(Cents(..))

    subtractCents (Cents 3) (Cents 1)
    --> Cents 2

-}
subtract : Cents -> Cents -> Cents
subtract a b =
    map2 (-) a b


gt : Cents -> Cents -> Bool
gt a b =
    map2E (>) a b

gte : Cents -> Cents -> Bool
gte a b =
   map2E (>=) a b