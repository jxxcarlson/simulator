
# Money

A new model for money with named currencies that 
can be either fiat or complementary, and which
have issue and expiration times.  The latter may be 
finite or infinite.

```elm
type Money =
      Money { amount : Cents
            , currency : Currency
            , issuedAt : BankTime
            , expiresAt : Expiration
            }
```            
One creates money this way:

```elm
    greenBucks : Currency
    greenBucks = createCompCurrency "Greenbucks"

    m : Money
    m = createFinite greenBucks 0 365 100.21

    toString m
    --> "100.21 Greenbucks (C) 0:365"
```


And and account like this:

```elm
    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime 0) |> valueToString
    --> "100.21 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime -1) |> valueToString
    --> "0 Greenbucks (C)"

    createAccountWithCurrency greenBucks [m] 
       |> value (bankTime 366) |> valueToString
    --> "0 Greenbucks (C)" 
```

Notice that the money created for this currency has an issue date and
and expiration date. An account with holdings of this 
valued at times before or after the expiration date has zero value.  

See the documentation for module `Currency` for more information, e,g,, 
how credit or debit an account.

**Note:** This is work in progress.

