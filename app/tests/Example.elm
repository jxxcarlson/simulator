module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



suite : Test
suite =
    Test.test "#payHouseholds: \n\n    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0\n    --> \"320 Real (F)\"" <|
            \() ->
                Expect.equal
                    (2 + 2)
                    ( 4 )
