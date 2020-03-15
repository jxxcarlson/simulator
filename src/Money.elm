module Money exposing
    ( Money, Value, bankTime, toString, valueToString
    , Cents, BankTime, Expiration, Currency
    , createCompCurrency, createFiatCurrency, createFinite, createInfinite
    , value
    , createValue, currency, getBankTime, mul, typeOfCurrency, typeOfMoney
    )

{-| A model for currency with an identity
and an expiration.


## Money, Account, and Value

@docs Money, Account, Value, bankTime, toString, valueToString


## Component of Money

@docs Cents, BankTime, Expiration, Currency


## Creating things

@docs createCompCurrency, createFiatCurrency, createFinite, createInfinite, createAccountWithCurrency, emptyAccount


## Operations on Accounts

@docs value, credit, debit

-}

import Internal.Money as Money
import Internal.Types as Type
    exposing
        ( BankTime
        , Cents
        , Currency
        , CurrencyType(..)
        , Expiration
        , Money
        , Value
        )
import Internal.Value as Value


{-| Money is the fundamental type of this module.
A value of type Money has an amount, a type (Fiat or Complementary).
a time at which it was issued, and an expiration time,
which is either Infinite or Finite BankTime

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m : Money
    m = createFinite greenBucks 0 365 100.21

    toString m
    --> "100.21 Greenbucks (C) 0:365"

-}
type alias Money =
    Type.Money


{-| An account at a given time has a Value

    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m1 : Money
    m1 = createFinite greenBucks 0 365 100.00

    m2 : Money
    m2 = createInFinite greenBucks 0 50.00

    acct : Account
    acct = createAccountWithCurrency greenBucks [m1, m2]
    value (bankTime 0) acct |> valueToString
    --> "150 Greenbucks (C)"

-}
type alias Value =
    Type.Value


createValue : Currency -> Float -> Value
createValue =
    Value.create


{-| Return a string representation of a Value
-}
valueToString : Value -> String
valueToString =
    Money.valueToString


{-| Create a BankTime value from an Integer
-}
bankTime : Int -> BankTime
bankTime =
    Money.bankTime


getBankTime : BankTime -> Int
getBankTime =
    Money.getBankTime


{-| We denominate money in integer Cents so as
to avoid round-off error.
-}
type alias Cents =
    Type.Cents


{-| BankTime t is an abstract integer time which
can be seconds, days, weeks, whatever
-}
type alias BankTime =
    Type.BankTime


{-| A currency may be finite or infinite,
that is, expiring or non-expiring.
-}
type alias Expiration =
    Type.Expiration


{-| Currency can be fiat or complementary
-}
type alias Currency =
    Type.Currency


type alias CurrencyType =
    Type.CurrencyType


{-| Create a complementary currency with given name
-}
createCompCurrency : String -> Currency
createCompCurrency =
    Money.createCompCurrency


{-| Create a fiat currency with given name
-}
createFiatCurrency : String -> Currency
createFiatCurrency =
    Money.createFiatCurrency


{-| Create money with an expiration date

greenBucks = createCompCurrency "GreenBucks"
m1 = createFinite greenBucks 0 365 100.01

-}
createFinite : Currency -> Int -> Int -> Float -> Money
createFinite =
    Money.createFinite


{-| Create money which does not expire
-}
createInfinite : Currency -> Int -> Float -> Money
createInfinite =
    Money.createInfinite


{-| A string representation of Money
-}
toString : Money -> String
toString =
    Money.stringFromMoney


currency : Money -> Currency
currency =
    Money.currency


typeOfMoney : Money -> CurrencyType
typeOfMoney =
    Money.currencyType


typeOfCurrency : Currency -> CurrencyType
typeOfCurrency currency_ =
    Money.ctype currency_


value : BankTime -> Money -> Value
value =
    Money.value


mul : Int -> Money -> Money
mul =
    Money.mul
