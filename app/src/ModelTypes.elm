module ModelTypes exposing (Item(..), Inventory, InventoryError(..), itemName, itemPrice, setQuantity, itemQuantity)
import Money

{-|  In this model, we assume that the name
and price of an item are constant.  That is,
the operations of `addItem` and `subItem`
do not affect these quantities.


-}
type Item = Item {
     name : String
   , price : Money.Value
   , quantity : Int}

itemName : Item -> String
itemName (Item i) =
    i.name

itemPrice : Item -> Money.Value
itemPrice (Item i) =
    i.price


itemQuantity : Item -> Int
itemQuantity (Item i) =
    i.quantity

setQuantity : Int -> Item -> Item
setQuantity q (Item data) =
   Item { data | quantity = q}



type alias Inventory = List Item


type InventoryError = InventoryOpSuccess | NoSuchItem | InsufficientQuantity
