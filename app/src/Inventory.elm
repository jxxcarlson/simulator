module Inventory exposing (add, sub, quantity, price, getItemQuantity)

import ModelTypes exposing(Item, Inventory, InventoryError(..))
import Internal.Types exposing(Value)
import Internal.Inventory exposing(name, price, quantity, mapItem)

{-|

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1  }

    add i1 []
    --> [i1]

    add i1 [i1]
    --> [Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2  }]

-}
add : Item -> Inventory -> Inventory
add item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> item::inventory
       Just target ->
           List.map (\targetItem -> Internal.Inventory.addToItem item targetItem) inventory


{-|

    import Internal.Types exposing(..)
    import Internal.Money
    import ModelTypes exposing(Item(..))
    import ModelTypes exposing(InventoryError(..))

    i0 : Item
    i0 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 0 }

    i1 : Item
    i1 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 1 }

    i2 : Item
    i2 = Item {name = "AA", price = Value Internal.Money.usDollars (Cents 100), quantity = 2 }


    sub i1 []
    --> (NoSuchItem, [])

    sub i1 [i1]
    --> (InventoryOpSuccess, [i0])

    sub i1 [i2]
    --> (InventoryOpSuccess, [i1])

    sub i2 [i1]
    --> (InsufficientQuantity, [i0])

-}
sub : Item -> Inventory -> (InventoryError, Inventory)
sub item inventory  =
   case List.filter (\i -> name i == name item && price i == price item) inventory |> List.head of
       Nothing -> (NoSuchItem, inventory)
       Just target ->
           if quantity target >= quantity item then
             (InventoryOpSuccess, List.map (\targetItem -> Internal.Inventory.subFromItem item targetItem) inventory)
           else
             let
                 adjustedItem = mapItem (\q -> quantity target) item
             in
                 (InsufficientQuantity, List.map (\targetItem -> Internal.Inventory.subFromItem adjustedItem targetItem) inventory)


quantity : Item -> Int
quantity = Internal.Inventory.quantity

price : Item -> Value
price = Internal.Inventory.price

getItemQuantity : Item -> Inventory -> Int
getItemQuantity item inventory =
   inventory
   |> List.filter (\i -> name i == name item && price i == price item)
   |> List.head
   |> Maybe.map quantity
   |> Maybe.withDefault 0