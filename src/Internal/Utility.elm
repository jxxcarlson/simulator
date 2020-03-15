
module Internal.Utility exposing (andOfList)

{-|

    andOfList [ ]
    --> True

    andOfList [True, True]
    --> True

    andOfList [True, False ]
    --> False

-}
andOfList : List Bool -> Bool
andOfList list =
  case list of
      [] -> True
      (x::rest) -> x && (andOfList rest)


