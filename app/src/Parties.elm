module Parties exposing (..)

import Entity exposing(Entity)
import Money exposing(Money, BankTime)
import ModelTypes exposing(Item, InventoryError(..))
import Account
import Value
import Internal.Utility as Utility
import Inventory


type Parties = Parties PartiesData

type alias PartiesData = {buyer : Entity, seller : Entity}

type TransactionStatus = TSuccess | TSFailure String


buyer : Parties -> Entity
buyer (Parties data) =
   data.buyer

seller : Parties -> Entity
seller (Parties  data) =
   data.seller


buy : BankTime -> Money -> Item -> Parties -> (TransactionStatus, Parties)
buy bt money item parties =
   let
     b = buyer parties
     s = seller parties
     maybeBuyerAccount = Entity.selectAccount money b
     maybeSellerAccount = Entity.selectAccount money s
     buyerInventory = Entity.inventory b
     sellerInventory = Entity.inventory s
   in
   case (maybeBuyerAccount, maybeSellerAccount) of
       (Just buyerAccount, Just sellerAccount ) ->
           let
               conditions =
                   [ Value.gte (Account.value bt buyerAccount) (Money.value bt money) |> Maybe.withDefault False -- buyer has sufficient funds
                   , (Money.value bt money) == (Value.mul (Inventory.quantity item) (Inventory.price item))   -- buyer tendered amount == cost of goods
                   , Inventory.quantity item   >=  Inventory.getItemQuantity item sellerInventory-- Seller has sufficient inventory
                   ]
           in
               case Utility.andOfList conditions of
                   False -> (TSFailure "Insufficient funds or something else", parties)
                   True ->
                     let
                       updatedSellerAccount = Account.credit bt money sellerAccount
                       updatedBuyerAccount = Account.debit bt money buyerAccount
                       (status, updatedSellerInventory) = Inventory.sub item sellerInventory

                     in
                       case status of
                           InventoryOpSuccess ->
                               let
                                  updatedBuyerInventory = Inventory.add item buyerInventory
                                  b2 = b |> Entity.updateAccount updatedBuyerAccount |> Entity.setInventory updatedBuyerInventory
                                  s2 = s |> Entity.updateAccount updatedSellerAccount |> Entity.setInventory updatedSellerInventory
                                in
                                  (TSuccess, Parties {seller = s2, buyer = b2})
                           _ -> (TSFailure "Insufficient inventory or something else", parties)



