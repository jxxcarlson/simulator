module Action exposing
    ( businessBuysGoods
    , businessPaysRent
    , consumeA
    , dailyActivity
    , householdBuysGoods
    , payHouseholds
    , readEducationalContent
    , recordData
    )

import Account
import ActionHelper as AH
import EngineData exposing (CCEarnings(..), Config)
import Entity exposing (Entity)
import Internal.Types exposing (Expiration(..))
import Inventory
import List.Extra
import ModelTypes exposing (Inventory)
import Money
import Random
import Report
import State exposing (BusinessLog, State)
import Utility
import Value


{-|

    import TestData
    import Report

    payHouseholds 0 TestData.initialState |> Report.fiatHoldingsDisplay 0
    --> "320 Real (F)"

-}
payHouseholds : Config -> Int -> State -> State
payHouseholds config t state =
    if List.member (modBy 30 t) state.config.householdPayDays then
        let
            households =
                AH.creditHouseHolds config t state.households
        in
        { state | households = households }

    else
        state


dailyActivity : Int -> State -> State
dailyActivity t state =
    state


consumeA : Int -> State -> State
consumeA t state =
    if List.member (modBy 30 t) state.config.householdConsumptionDays then
        let
            reduceInventoryOfA : Inventory -> Inventory
            reduceInventoryOfA inventory =
                Inventory.sub state.config.itemA inventory |> Tuple.second

            reduceInventoryOfHouseHold : Entity -> Entity
            reduceInventoryOfHouseHold e =
                if Entity.inventoryAmount "AA" e > 0 then
                    Entity.mapInventory reduceInventoryOfA e

                else
                    e

            positiveInventoryHouseholds =
                List.filter (\e -> Entity.inventoryAmount "AA" e > 0) state.households

            newHouseholds =
                List.map reduceInventoryOfHouseHold state.households
        in
        { state
            | households = newHouseholds
            , totalHouseholdConsumption = state.totalHouseholdConsumption + List.length positiveInventoryHouseholds
        }

    else
        state


initializeSupplier : State -> State
initializeSupplier state =
    state


readEducationalContent : State -> State
readEducationalContent state =
    case state.config.ccEarnings of
        CCEarningsOFF ->
            state

        CCEarningsON ->
            case modBy state.config.educationalContentCycle state.tick == 3 of
                False ->
                    state

                True ->
                    let
                        config =
                            state.config

                        exp =
                            config.complementaryCurrencyExpiration

                        amount =
                            config.educationPaymentPerCycle

                        earnCC : Entity -> Entity
                        earnCC e =
                            AH.creditEntity config state.tick config.complementaryCurrency exp amount e

                        newBusinesses =
                            List.map earnCC state.businesses

                        n =
                            List.length newBusinesses

                        amountEarned =
                            toFloat n * amount

                        logString =
                            "Earn CC for " ++ String.fromInt n ++ " biz: " ++ String.fromFloat amountEarned
                    in
                    { state | businesses = newBusinesses, log = logItem state logString }


probability : Random.Generator Float
probability =
    Random.float 0 1



-- MAIN: BUSINESS BUY GOODS
-- Entity.inventoryAmount "AA" e


businessBuysGoods : State -> State
businessBuysGoods state =
    case randomBusiness state of
        ( Nothing, _ ) ->
            state

        ( Just business, seed ) ->
            if Entity.inventoryAmount "AA" business < state.config.maxInventory then
                buy_ state seed business

            else
                { state | seed = seed }


buy_ state seed business =
    let
        ( purchaseAmt, seed2 ) =
            purchaseAmount state seed

        fiatBalance =
            Entity.getFiatAccountFloatValue state.tick business

        purchaseCost =
            toFloat purchaseAmt * state.config.itemCost

        maximumPurchaseAmount =
            min fiatBalance purchaseCost

        adjustedPurchaseAmount =
            floor (maximumPurchaseAmount / state.config.itemCost)

        aCC : Int
        aCC =
            state.config.maximumCCRatio * toFloat adjustedPurchaseAmount |> round

        aFiat : Int
        aFiat =
            adjustedPurchaseAmount - aCC

        item =
            ModelTypes.setQuantity purchaseAmt state.config.itemA

        newBusiness =
            let
                config =
                    state.config
            in
            -- subtract total cost of items purchased from supplier
            Entity.addToInventory item business
                |> AH.creditEntity config state.tick config.fiatCurrency Infinite (toFloat aFiat * -config.itemCost)
                |> AH.creditEntity config state.tick config.complementaryCurrency config.complementaryCurrencyExpiration (toFloat aCC * -config.itemCost)

        newBusinesses =
            List.Extra.updateIf
                (\b -> Entity.getName b == Entity.getName newBusiness)
                (\b -> newBusiness)
                state.businesses

        logString =
            getLogString purchaseAmt business newBusiness
    in
    { state | seed = seed2, businesses = newBusinesses, log = logItem state logString }



-- HELPERS: FOR BUSINESS BUY GOODS


selectRandomBusiness : State -> ( Maybe Entity, Random.Seed )
selectRandomBusiness state =
    let
        ( p, newSeed ) =
            Random.step probability state.seed
    in
    if p > state.config.probabilityOfPurchasing then
        ( Nothing, newSeed )

    else
        Utility.randomElement newSeed state.businesses


purchaseAmount : State -> Random.Seed -> ( Int, Random.Seed )
purchaseAmount state seed =
    let
        ( p, newSeed ) =
            Random.step probability seed

        randomPurchaseAmount : Float -> Int
        randomPurchaseAmount q =
            let
                range =
                    toFloat (state.config.maximumPurchaseOfA - state.config.minimumPurchaseOfA)
            in
            state.config.minimumPurchaseOfA + round (q * range)
    in
    ( randomPurchaseAmount p, newSeed )


{-| Returns the maximum cumulative amount of goods that a business can purchase by a given data,
where the date is given by state.tick
-}
businessPurchaseCeiling : State -> Int
businessPurchaseCeiling state =
    round (toFloat (state.config.monthlyPurchaseCeilingInUnits * state.tick) / 30.0) + state.config.monthlyPurchaseCeilingHeadRoom


{-| Candidate business which can purchase goods
-}
randomBusiness : State -> ( Maybe Entity, Random.Seed )
randomBusiness state =
    let
        ( business__, newSeed ) =
            selectRandomBusiness state

        fractionalPurchaseCeilingToDate =
            businessPurchaseCeiling state
    in
    case Maybe.andThen Entity.unitsPurchased business__ |> Maybe.map (\x -> x < fractionalPurchaseCeilingToDate) of
        Just True ->
            ( business__, newSeed )

        _ ->
            ( Nothing, newSeed )


getLogString : Int -> Entity -> Entity -> String
getLogString purchaseAmt_ business_ newBusiness_ =
    let
        oldFiatBalance =
            Report.fiatBalanceOfEntity (Money.bankTime 0) business_ |> String.fromFloat

        newFiatBalance =
            Report.fiatBalanceOfEntity (Money.bankTime 0) newBusiness_ |> String.fromFloat
    in
    "Biz " ++ Entity.getName newBusiness_ ++ " buy " ++ String.fromInt purchaseAmt_ ++ ", bal: " ++ oldFiatBalance ++ " >> " ++ newFiatBalance


logItem : State -> String -> List String
logItem state item =
    (String.padRight 3 ' ' (String.fromInt state.tick) ++ ": " ++ item) :: state.log



{- MAIN: BUSINESS PAYS RENT



-}


businessPaysRent : Int -> State -> State
businessPaysRent t state =
    let
        debit =
            AH.creditEntity state.config state.tick state.config.fiatCurrency Infinite -state.config.businessRent
    in
    case modBy 30 t == state.config.rentDueDate of
        True ->
            { state | businesses = List.map debit state.businesses }

        False ->
            state



-- MAIN: HOUSEHOLD BUY GOODS


{-| Choose a low-inventory household at random and buy goods
-}
householdBuysGoods : Int -> State -> State
householdBuysGoods t state =
    let
        -- TODO: This let block needs some thought
        sortByAccountValue : Entity -> Int
        sortByAccountValue e =
            Entity.getFiatAccount e
                |> Account.value (Money.bankTime t)
                |> Value.intValue
                |> (\v -> -v)

        n =
            List.length state.households

        orderedHouseholds =
            List.sortBy (\e -> sortByAccountValue e) state.households
                |> List.take (n // 2)

        ( needyHousehold, newSeed ) =
            Utility.randomElement state.seed orderedHouseholds
    in
    case needyHousehold of
        Nothing ->
            { state | seed = newSeed }

        Just e ->
            householdBuysGoods_ t e { state | seed = newSeed }


householdBuysGoods_ : Int -> Entity -> State -> State
householdBuysGoods_ t household_ state =
    case findShopWithPositiveInventory state household_ of
        ( Nothing, newBusinessLog ) ->
            { state | businessLog = newBusinessLog }

        ( Just shop_, newBusinessLog ) ->
            let
                ( shop, message ) =
                    case Entity.inventoryAmount "AA" shop_ == 0 of
                        False ->
                            ( shop_, "" )

                        True ->
                            List.filter (\sh -> sh /= shop_) state.businesses
                                |> List.head
                                |> Maybe.withDefault shop_
                                |> (\s -> ( s, "InventoryFailure" ))

                ( amountToPurchase, newSeed ) =
                    amountToPurchaseFromShop state shop household_

                ( newHousehold, newBusiness ) =
                    buyItem t state amountToPurchase household_ shop_

                newHouseholds =
                    List.Extra.updateIf
                        (\e1 -> Entity.getName e1 == Entity.getName household_)
                        (\_ -> newHousehold)
                        state.households

                newBusinesses =
                    List.Extra.updateIf
                        (\e1 -> Entity.getName e1 == Entity.getName shop)
                        (\_ -> newBusiness)
                        state.businesses

                logString =
                    case message of
                        "InventoryFailure" ->
                            "H" ++ Entity.getName newHousehold ++ " buy " ++ String.fromInt amountToPurchase ++ " from ((" ++ Entity.getName newBusiness ++ "))"

                        _ ->
                            "H" ++ Entity.getName newHousehold ++ " buy " ++ String.fromInt amountToPurchase ++ " from " ++ Entity.getName newBusiness
            in
            { state
                | households = newHouseholds
                , businesses = newBusinesses
                , seed = newSeed
                , totalHouseholdPurchases = state.totalHouseholdPurchases + amountToPurchase
                , log = logItem state logString
                , businessLog = newBusinessLog
            }



-- HELPERS: HOUSEHOLD BUYS GOODS
-- (1) findShopWithPositiveInventory


findShopWithPositiveInventory : State -> Entity -> ( Maybe Entity, List BusinessLog )
findShopWithPositiveInventory state household =
    let
        businesses =
            List.sortBy (\e -> Entity.distance e household) state.businesses
    in
    loop { shops = businesses, log = state.businessLog } nextState


nextState : SState -> Step SState ( Maybe Entity, List BusinessLog )
nextState st =
    case List.head st.shops of
        Nothing ->
            Done ( Nothing, st.log )

        Just shop ->
            case Entity.inventoryAmount "AA" shop == 0 of
                True ->
                    Loop { shops = List.drop 1 st.shops, log = incrementLostSales shop st.log }

                False ->
                    Done ( Just shop, st.log )


incrementLostSales : Entity -> List BusinessLog -> List BusinessLog
incrementLostSales business log =
    let
        updater bLog =
            if Entity.getName business == bLog.name then
                { bLog | lostSales = bLog.lostSales + 1 }

            else
                bLog
    in
    List.map updater log


type Step state a
    = Done a
    | Loop state


type alias SState =
    { shops : List Entity, log : List BusinessLog }


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b



-- (2) Other helpers


selectRandomHousehold : State -> ( Maybe Entity, Random.Seed )
selectRandomHousehold state =
    let
        ( _, newSeed ) =
            Random.step probability state.seed
    in
    Utility.randomElement state.seed state.businesses


householdPurchaseAmount : Random.Seed -> State -> ( Int, Random.Seed )
householdPurchaseAmount seed state =
    let
        ( p, newSeed ) =
            Random.step probability seed

        range =
            toFloat (state.config.householdMaximumPurchaseAmount - state.config.householdMinimumPurchaseAmount)
    in
    ( state.config.householdMinimumPurchaseAmount + round (p * range), newSeed )


amountToPurchaseFromShop state shop__ household__ =
    let
        shopInventoryAmt =
            Entity.inventoryAmount "AA" shop__

        householdInventoryAmt =
            Entity.inventoryAmount "AA" household__

        ( purchaseAmt, newSeed_ ) =
            householdPurchaseAmount state.seed state
    in
    if householdInventoryAmt >= state.config.householdLowInventoryThreshold then
        -- Don't purchase itemA if already have enough on hand
        ( 0, newSeed_ )

    else if purchaseAmt > shopInventoryAmt then
        -- Can't purchase more than is available in shop
        ( shopInventoryAmt, newSeed_ )

    else
        ( purchaseAmt, newSeed_ )


buyItem : Int -> State -> Int -> Entity -> Entity -> ( Entity, Entity )
buyItem t state amountToPurchase_ household_ shop_ =
    let
        item =
            ModelTypes.setQuantity amountToPurchase_ state.config.itemA

        itemPrice =
            Money.mul amountToPurchase_ state.config.itemAMoney

        addInventoryOfA : Inventory -> Inventory
        addInventoryOfA inventory =
            Inventory.add item inventory

        addInventoryOfEntity : Entity -> Entity
        addInventoryOfEntity e_ =
            Entity.mapInventory addInventoryOfA e_

        subInventoryOfA : Inventory -> Inventory
        subInventoryOfA inventory =
            Inventory.sub item inventory
                |> Tuple.second

        subInventoryOfEntity : Entity -> Entity
        subInventoryOfEntity e_ =
            Entity.mapInventory subInventoryOfA e_

        debitAccount : Account.Account -> Account.Account
        debitAccount =
            \account -> Account.debit (Money.bankTime t) itemPrice account

        creditAccount : Account.Account -> Account.Account
        creditAccount =
            \account -> Account.credit (Money.bankTime t) itemPrice account

        newHousehold_ =
            addInventoryOfEntity household_
                |> Entity.mapAccount debitAccount (Entity.getFiatAccount household_)

        newShop_ =
            subInventoryOfEntity shop_
                |> Entity.mapAccount creditAccount (Entity.getFiatAccount shop_)
    in
    ( newHousehold_, newShop_ )



-- CALLED EXTERNALLY


recordData : Int -> State -> State
recordData tick state =
    let
        i =
            Report.businessInventoryOf "AA" state |> List.head |> Maybe.withDefault 0 |> toFloat

        t =
            toFloat tick
    in
    { state | data = ( t, i ) :: state.data }
