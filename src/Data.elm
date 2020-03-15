module Data exposing (..)

import Money exposing(..)


c1 =
    { amount = 1, issueTime = 0, expiration = Infinite }


c2 =
    { amount = 1, issueTime = 5, expiration = Infinite }


c3 =
    { amount = 1, issueTime = 88, expiration = Infinite }


m1 =
    { amount = 10, issueTime = 0, expiration = Infinite }


m2 =
    { amount = 10, issueTime = 5, expiration = Infinite }


acct =
    [ m1, m2 ]
