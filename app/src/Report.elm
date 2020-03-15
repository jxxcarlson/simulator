module Report exposing
    ( businessInventoryOf
    , ccBalanceOfEntity
    , ccBalanceOfEntityList
    , fiatBalanceOfEntity
    , fiatBalanceOfEntityList
    , fiatHoldingsDisplay
    , householdInventoryOf
    , minMaxHouseholdInventoryOf
    , numberOfInventoriesBelow
    )

import Account
import Entity exposing (Entity)
import Internal.Types exposing (BankTime)
import Maybe.Extra
import ModelTypes exposing (itemName, itemQuantity)
import Money
import State exposing (State)
import Value


fiatHoldingsDisplay t state =
    case Entity.fiatHoldingsOEntities t state.households of
        Just value ->
            Money.valueToString value

        Nothing ->
            "--"


householdInventoryOf : String -> State -> Int
householdInventoryOf itemName_ state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum
    in
    List.map f state.households
        |> List.sum


minMaxHouseholdInventoryOf : String -> State -> List Int
minMaxHouseholdInventoryOf itemName_ state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum

        inventories : List Int
        inventories =
            List.map f state.households
    in
    [ List.minimum inventories, List.maximum inventories ]
        |> Maybe.Extra.values


numberOfInventoriesBelow : String -> Int -> State -> Int
numberOfInventoriesBelow itemName_ k state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum

        inventories : List Int
        inventories =
            List.map f state.households
    in
    List.filter (\i -> i <= k) inventories |> List.length


businessInventoryOf : String -> State -> List Int
businessInventoryOf itemName_ state =
    let
        f : Entity -> Int
        f e =
            List.filter (\i -> itemName i == itemName_) (Entity.inventory e)
                |> List.map itemQuantity
                |> List.sum
    in
    List.map f state.businesses


fiatBalanceOfEntityList : BankTime -> List Entity -> List Float
fiatBalanceOfEntityList bt entityList =
    let
        f : Entity -> Float
        f e =
            e
                |> Entity.getFiatAccount
                |> Account.value bt
                |> Value.toFloat_
    in
    List.map f entityList


fiatBalanceOfEntity : BankTime -> Entity -> Float
fiatBalanceOfEntity bt entity =
    entity
        |> Entity.getFiatAccount
        |> Account.value bt
        |> Value.toFloat_


ccBalanceOfEntityList : BankTime -> List Entity -> List Float
ccBalanceOfEntityList bt entityList =
    let
        f : Entity -> Float
        f e =
            e
                |> Entity.getCCAccount
                |> Account.value bt
                |> Value.toFloat_
    in
    List.map f entityList


ccBalanceOfEntity : BankTime -> Entity -> Float
ccBalanceOfEntity bt entity =
    entity
        |> Entity.getCCAccount
        |> Account.value bt
        |> Value.toFloat_
