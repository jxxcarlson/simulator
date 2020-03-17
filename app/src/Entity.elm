module Entity exposing
    ( BusinessCharRecord
    , Characteristics(..)
    , Common
    , Entity(..)
    , HouseholdCharRecord
    , TEntity(..)
    , addToInventory
    , complementaryAccount
    , distance
    , fiatAccount
    , fiatHoldingsOEntities
    , getCCAccount
    , getColor
    , getFiatAccount
    , getFiatAccountFloatValue
    , getName
    , getPosition
    , getType
    , inventory
    , inventoryAmount
    , inventorySize
    , mapAccount
    , mapInventory
    , selectAccount
    , setCCAccount
    , setColor
    , setFiatAccount
    , setInventory
    , setName
    , setPosition
    , unitsPurchased
    , updateAccount
    )

{-|

     business : Entity
     business = Entity
        { name = "AAA Bakery"
        , entityType = TBusiness
        , account = Money.emptyAccount cambiatus
        , inventory = []
        , position = Position 0 0
        , color = Color.rgb 1 0 0
       }
       (BusinessCharacteristics { radius = 10.0 })

-}

import Account exposing (Account)
import CellGrid exposing (Position)
import Color exposing (Color)
import Internal.Types exposing (CurrencyType(..))
import Inventory
import ModelTypes exposing (Inventory, Item)
import Money exposing (Money)
import Value


type Entity
    = Entity Common Characteristics


type alias Common =
    { name : String
    , entityType : TEntity
    , complementaryAccount : Account
    , fiatAccount : Account
    , inventory : Inventory
    , position : Position
    , color : Color
    }


inventory : Entity -> List Item
inventory (Entity common _) =
    common.inventory


mapInventory : (Inventory -> Inventory) -> Entity -> Entity
mapInventory f (Entity common characterstics) =
    Entity { common | inventory = f common.inventory } characterstics


complementaryAccount : Entity -> Account
complementaryAccount (Entity common _) =
    common.complementaryAccount


fiatAccount : Entity -> Account
fiatAccount (Entity common _) =
    common.fiatAccount


selectAccount : Money -> Entity -> Maybe Account
selectAccount money (Entity common _) =
    if Money.currency money == Account.currency common.fiatAccount then
        Just <| common.fiatAccount

    else if Money.currency money == Account.currency common.complementaryAccount then
        Just <| common.complementaryAccount

    else
        Nothing


type TEntity
    = TShop
    | TSupplier
    | THousehold
    | TEducator


type Characteristics
    = BusinessCharacteristics BusinessCharRecord
    | EducatorCharacteristics EducatorCharRecord
    | SupplierCharacteristics SupplierCharRecord
    | HouseholdCharacteristics HouseholdCharRecord


unitsPurchased : Entity -> Maybe Int
unitsPurchased (Entity _ characteristics) =
    case characteristics of
        BusinessCharacteristics data ->
            Just data.unitsPurchased

        _ ->
            Nothing


type alias BusinessCharRecord =
    { radius : Float
    , unitsPurchased : Int
    }


type alias EducatorCharRecord =
    {}


type alias SupplierCharRecord =
    {}


type alias HouseholdCharRecord =
    { monthlyConsumptionA : Int
    }


fiatHoldingsOEntities : Int -> List Entity -> Maybe Money.Value
fiatHoldingsOEntities t list =
    let
        mergedAccounts =
            list
                |> List.map getFiatAccount
                |> Account.merge
    in
    case mergedAccounts of
        Just accounts ->
            Just <| Account.value (Money.bankTime t) accounts

        Nothing ->
            Nothing


positionDistance : Position -> Position -> Float
positionDistance p q =
    let
        deltaX =
            p.row - q.row |> toFloat

        deltaY =
            p.column - q.column |> toFloat
    in
    sqrt (deltaX * deltaX + deltaY * deltaY)


distance : Entity -> Entity -> Float
distance (Entity common1 _) (Entity common2 _) =
    positionDistance common1.position common2.position


getFiatAccount : Entity -> Account
getFiatAccount (Entity common _) =
    common.fiatAccount


getFiatAccountFloatValue : Int -> Entity -> Float
getFiatAccountFloatValue t (Entity common _) =
    common.fiatAccount
        |> Account.value (Money.bankTime t)
        |> Value.toFloat_


setFiatAccount : Account -> Entity -> Entity
setFiatAccount account (Entity common char) =
    Entity { common | fiatAccount = account } char


getCCAccount : Entity -> Account
getCCAccount (Entity common _) =
    common.complementaryAccount


setCCAccount : Account -> Entity -> Entity
setCCAccount account (Entity common char) =
    Entity { common | complementaryAccount = account } char


updateAccount : Account -> Entity -> Entity
updateAccount account e =
    case Account.currencyType account of
        Fiat ->
            setFiatAccount account e

        Complementary ->
            setCCAccount account e


mapAccount : (Account -> Account) -> Account -> Entity -> Entity
mapAccount f account e =
    case Account.currencyType account of
        Fiat ->
            setFiatAccount (f account) e

        Complementary ->
            setCCAccount (f account) e


setInventory : Inventory -> Entity -> Entity
setInventory inventory_ (Entity common characteristics) =
    Entity { common | inventory = inventory_ } characteristics


addToInventory : Item -> Entity -> Entity
addToInventory item e =
    mapInventory (\inv -> Inventory.add item inv) e


addToUnitsPurchased : Int -> Entity -> Entity
addToUnitsPurchased k ((Entity common char) as e) =
    case char of
        BusinessCharacteristics data ->
            Entity common (BusinessCharacteristics { data | unitsPurchased = data.unitsPurchased + k })

        _ ->
            e


getName : Entity -> String
getName (Entity common _) =
    common.name


setName : String -> Entity -> Entity
setName name (Entity common characteristics) =
    Entity { common | name = name } characteristics


getType : Entity -> TEntity
getType (Entity common _) =
    common.entityType


getPosition : Entity -> Position
getPosition (Entity common characteristics) =
    common.position


setPosition : Int -> Int -> Entity -> Entity
setPosition i j (Entity common characteristics) =
    Entity { common | position = Position i j } characteristics


getColor : Entity -> Color
getColor (Entity common characteristics) =
    common.color


setColor : Float -> Float -> Float -> Entity -> Entity
setColor r g b (Entity common characteristics) =
    Entity { common | color = Color.rgb r g b } characteristics


inventorySize : Entity -> Int
inventorySize (Entity common _) =
    List.length common.inventory


inventoryAmount : String -> Entity -> Int
inventoryAmount itemName_ e =
    List.filter (\i -> ModelTypes.itemName i == itemName_) (inventory e)
        |> List.map ModelTypes.itemQuantity
        |> List.sum
