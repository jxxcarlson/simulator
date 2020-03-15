module Internal.Inventory exposing (mapItem, map2Item, name, price, quantity, addToItem , subFromItem)


import Money
import Basics
import ModelTypes exposing(Item(..), Inventory)



name : Item -> String
name (Item data) =
    data.name

price : Item -> Money.Value
price (Item data) =
    data.price

quantity : Item -> Int
quantity (Item data) =
    data.quantity



{-|

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    mapItem (\x -> 2*x) i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

-}
mapItem : (Int -> Int) -> Item -> Item
mapItem  f (Item data) =
    Item { data | quantity =  f data.quantity }


{-|

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    map2Item (+) i1 i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

-}
map2Item : (Int -> Int -> Int ) -> Item -> Item -> Item
map2Item  f (Item data1) (Item data2) =
    Item { data1 | quantity =  f data1.quantity data2.quantity }


{-|  If the name and price of the target agree,
add the quantity of the source to the target
and return the target. Otherwise, return the target.

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1 }

    i2 : Item
    i2 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }

    jj : Item
    jj = Item {name = "BB", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }

    subFromItem i1 i2
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    subFromItem jj i2
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }
 -}
subFromItem : Item -> Item -> Item
subFromItem source target =
    case (name source == name target, price source == price target)  of
        (True, True) -> map2Item (-)  target source
        (_, _) -> target


{-|  If the name and price of the target agree,
add the quantity of the source to the target
and return the target. Otherwise, return the target.

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    i2 : Item
    i2 = Item {name = "B", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    addToItem i1 i1
    --> Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }

    addToItem i1 i2
    --> i2

-}
addToItem : Item -> Item -> Item
addToItem source target =
    case (name source == name target, price source == price target)  of
        (True, True) -> map2Item (+) source target
        (_, _) -> target

