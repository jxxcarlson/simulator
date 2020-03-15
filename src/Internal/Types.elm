module Internal.Types exposing (Money(..), Cents(..), Value(..), BankTime(..), Expiration(..), Currency(..), CurrencyType(..))



{-| Currency is the fundamental type of this module.
A Currency value has an amount, a type (Fiat or Complementary).
a time at which it was issued, and an expiration period,
which is either Infinite or Finite BankTime

    c1 = {amount = Cents 123, currency = greenBucks, issueTime = 0, expiration = Finite 100 }

-}
type Money =
    Money { amount : Cents
    , currency : Currency
    , issuedAt : BankTime
    , expiresAt : Expiration
    }


{-|  We denominate money in integer Cents so as
to avoid round-off error.
-}
type Cents = Cents Int

{-| An account at a given time has a Value

-}
type Value = Value Currency Cents



{-| BankTime t is an abstract integer time which
can be seconds, days, weeks, whatever
-}
type BankTime = BankTime Int


{-| A currency may be finite or infinite,
that is, expiring or non-expiring.

-}
type Expiration = Infinite | Finite BankTime


{-| Currency can be fiat or complementary
-}
type Currency = Currency CurrencyType String

type CurrencyType = Fiat | Complementary

