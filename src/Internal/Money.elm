module Internal.Money exposing
    ( amount
    , bankTime
    , consolidate
    , createCompCurrency
    , createCurrency
    , createFiatCurrency
    , createFinite
    , createInfinite
    , ctype
    , currency
    , currencyType
    , expiresAt
    , floatFromCents
    , getBankTime
    , getName
    , greenBucks
    , group
    , isValid
    , issuedAt
    , mul
    , negate
    , redBucks
    , sameCurrency
    , sameExpiration
    , sameIssue
    , samePeriod
    , stringFromBankTime
    , stringFromCents
    , stringFromCurrency
    , stringFromExpiration
    , stringFromMoney
    , usDollars
    , value
    , valueInCents_
    , valueToString
    )

{-| A model for currency with an identity
and an expiration.
-}

import Internal.Cents as Cents
import Internal.Types exposing (BankTime(..), Cents(..), Currency(..), CurrencyType(..), Expiration(..), Money(..), Value(..))
import Internal.Utility as Utility
import List.Extra
import String.Interpolate exposing (interpolate)


bankTime : Int -> BankTime
bankTime t =
    BankTime t


getBankTime : BankTime -> Int
getBankTime (BankTime t) =
    t


createCurrency : CurrencyType -> String -> Currency
createCurrency ctype_ name =
    Currency ctype_ name


createCompCurrency : String -> Currency
createCompCurrency name =
    Currency Complementary name


createFiatCurrency : String -> Currency
createFiatCurrency name =
    Currency Fiat name


getName : Currency -> String
getName (Currency _ name_) =
    name_


greenBucks : Currency
greenBucks =
    createCompCurrency "Greenbucks"


redBucks : Currency
redBucks =
    createCompCurrency "Redbucks"


usDollars : Currency
usDollars =
    createFiatCurrency "USDollars"


createFinite : Currency -> Int -> Int -> Float -> Money
createFinite currency_ issuedAt_ expiresAt_ amount_ =
    Money
        { currency = currency_
        , issuedAt = BankTime issuedAt_
        , expiresAt = Finite (BankTime expiresAt_)
        , amount = Cents (round (100.0 * amount_))
        }


createInfinite : Currency -> Int -> Float -> Money
createInfinite currency_ issuedAt_ amount_ =
    Money
        { currency = currency_
        , issuedAt = BankTime issuedAt_
        , expiresAt = Infinite
        , amount = Cents (round (100.0 * amount_))
        }


{-|

    import Internal.Types exposing(..)

    sameCurrency []
    --> False

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    redBucks : Currency
    redBucks = createCompCurrency "Redbucks"

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.21

    m2 : Money
    m2 = createFinite redBucks 0 365 100.21

    sameCurrency [m1, m1]
    --> True

    sameCurrency [m1, m2]
    --> False

-}
sameCurrency : List Money -> Bool
sameCurrency list =
    case list of
        [] ->
            False

        x :: rest ->
            let
                headCurrency =
                    currency x
            in
            rest
                |> List.map (\m -> headCurrency == currency m)
                |> Utility.andOfList


amount : Money -> Cents
amount (Money m) =
    m.amount


currency : Money -> Currency
currency (Money m) =
    m.currency


currencyType : Money -> CurrencyType
currencyType (Money m) =
    m.currency |> ctype


ctype : Currency -> CurrencyType
ctype (Currency t _) =
    t


issuedAt : Money -> BankTime
issuedAt (Money m) =
    m.issuedAt


expiresAt : Money -> Expiration
expiresAt (Money m) =
    m.expiresAt


sameIssue : Money -> Money -> Bool
sameIssue a b =
    issuedAt a == issuedAt b


sameExpiration : Money -> Money -> Bool
sameExpiration a b =
    expiresAt a == expiresAt b


{-|

    import Internal.Types exposing(..)

    stringFromMoney <| Money  {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> "1.23 Greenbucks (C) 0:100"

    stringFromMoney <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Infinite }
    --> "1.23 Greenbucks (C) 0:Infinite"

-}
stringFromMoney : Money -> String
stringFromMoney (Money m) =
    interpolate "{0} {1} {2}:{3}"
        [ stringFromCents m.amount
        , stringFromCurrency m.currency
        , stringFromBankTime m.issuedAt
        , stringFromExpiration m.expiresAt
        ]



-- INFRASTRUCTURE BELOO HERE --


{-|

    import Internal.Types exposing(..)

    c1 : Money
    c1 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    Internal.Money.negate c1
    --> Money {amount = Cents -123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

-}
negate : Money -> Money
negate (Money m) =
    Money { m | amount = Cents.negate m.amount }


{-|

    import Internal.Types exposing(..)


    c1 : Money
    c1 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 10, expiresAt = Finite (BankTime 100) }

    c3 : Money
    c3 =  Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 200) }

    samePeriod c1 c1
    --> True

    samePeriod c1 c2
    --> False

    samePeriod c1 c3
    --> False

-}
samePeriod : Money -> Money -> Bool
samePeriod a b =
    issuedAt a == issuedAt b && expiresAt a == expiresAt b


{-|

    import Internal.Types exposing(..)

    c1 : Money
    c1 =  Money {amount = Cents 100, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 200, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c3 : Money
    c3 =  Money {amount = Cents 300, currency = greenBucks, issuedAt = BankTime 10, expiresAt = Finite (BankTime 100) }

    group [c1]
    --> [[c1]]

    group []
    --> []

    group [c1, c2, c3]
    --> [[c1,c2], [c3]]

-}
group : List Money -> List (List Money)
group list =
    list
        |> List.Extra.groupWhile samePeriod
        |> List.map (\item -> Tuple.first item :: Tuple.second item)


{-| This is DANGEROUS function. It assumes
that all elements of the list have the
same period ahd are of the same currency.
It should only be called by functions that
establish these conditions.

    import Internal.Types exposing(Money(..), BankTime(..), Cents(..), Expiration(..))

    c1 : Money
    c1 =  Money {amount = Cents 100, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c2 : Money
    c2 =  Money {amount = Cents 200, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    c3 : Money
    c3 =  Money {amount = Cents 300, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }

    consolidate [c1,c2]
    --> Just c3

-}
consolidate : List Money -> Maybe Money
consolidate list =
    case List.head list of
        Nothing ->
            Nothing

        Just (Money m) ->
            Just <| Money { m | amount = valueInCents_ list }



-- OPERATIONS AND FUNCTIONS --


{-|

    import Internal.Types exposing(Money(..), Cents(..), BankTime(..), Expiration(..))

    isValid (BankTime -1)  <| Money  {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> False

    isValid (BankTime 0) <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 1) <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 100) <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> True

    isValid (BankTime 101) <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Finite (BankTime 100) }
    --> False

    isValid (BankTime 101) <| Money {amount = Cents 123, currency = greenBucks, issuedAt = BankTime 0, expiresAt = Infinite }
    --> True

-}
isValid : BankTime -> Money -> Bool
isValid (BankTime currentTime) (Money m) =
    case m.issuedAt of
        BankTime issueTime ->
            if issueTime > currentTime then
                False

            else
                case m.expiresAt of
                    Infinite ->
                        True

                    Finite (BankTime expirationTime) ->
                        expirationTime >= currentTime


value : BankTime -> Money -> Value
value bt ((Money data) as m) =
    case isValid bt m of
        True ->
            Value data.currency data.amount

        False ->
            Value data.currency (Cents 0)



-- CONVERSIONS --


valueInCents_ : List Money -> Cents
valueInCents_ list =
    case list of
        [] ->
            Cents 0

        _ ->
            list
                |> List.map amount
                |> List.map (\(Cents k) -> k)
                |> List.sum
                |> (\s -> Cents s)


valueToString : Value -> String
valueToString (Value currency_ cents) =
    interpolate "{1} {0}" [ stringFromCurrency currency_, stringFromCents cents ]


{-|

    import Internal.Types exposing(Cents(..))

    stringFromCents (Cents 123)
    --> "1.23"

-}
stringFromCents : Cents -> String
stringFromCents cents =
    floatFromCents cents |> String.fromFloat


{-|

    import Internal.Types exposing(Cents(..))

    floatFromCents (Cents 123) |> String.fromFloat
    --> "1.23"

    floatFromCents (Cents -123) |> String.fromFloat
        --> "-1.23"

-}
floatFromCents : Cents -> Float
floatFromCents (Cents k) =
    toFloat k / 100.0


stringFromExpiration : Expiration -> String
stringFromExpiration e =
    case e of
        Finite t ->
            stringFromBankTime t

        Infinite ->
            "Infinite"


stringFromBankTime : BankTime -> String
stringFromBankTime (BankTime k) =
    String.fromInt k


stringFromCurrency : Currency -> String
stringFromCurrency (Currency ctype_ name) =
    case ctype_ of
        Fiat ->
            interpolate "{0} ({1})" [ name, "F" ]

        Complementary ->
            interpolate "{0} ({1})" [ name, "C" ]


mul : Int -> Money -> Money
mul k (Money data) =
    Money { data | amount = Cents.mul k data.amount }
