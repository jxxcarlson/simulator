module Action exposing
    ( businessBuyGoods
    , consumeA
    , dailyActivity
    , householdBuyGoods
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


businessBuyGoods : State -> State
businessBuyGoods state =
    let
        ( business__, seed2 ) =
            selectRandomBusiness state

        fractionalPurchaseCeilingToDate =
            round (toFloat (state.config.monthlyPurchaseCeilingInUnits * state.tick) / 30.0) + state.config.monthlyPurchaseCeilingHeadRoom

        business_ =
            case Maybe.andThen Entity.unitsPurchased business__ |> Maybe.map (\x -> x < fractionalPurchaseCeilingToDate) of
                Just True ->
                    business__

                _ ->
                    Nothing
    in
    case business_ of
        Nothing ->
            state

        Just business ->
            let
                ( a, seed3 ) =
                    purchaseAmount state seed2

                aCC =
                    config.maximumCCRatio * toFloat a |> round

                aFiat =
                    a - aCC

                item =
                    ModelTypes.setQuantity a state.config.itemA

                config =
                    state.config

                newBusiness =
                    -- subtract total cost of items purchased from supplier
                    Entity.addToInventory item business
                        |> AH.creditEntity config state.tick config.fiatCurrency Infinite (toFloat aFiat * -config.itemCost)
                        |> AH.creditEntity config state.tick config.complementaryCurrency config.complementaryCurrencyExpiration (toFloat aCC * -config.itemCost)

                newBusinesses =
                    List.Extra.updateIf
                        (\b -> Entity.getName b == Entity.getName newBusiness)
                        (\b -> newBusiness)
                        state.businesses

                oldFiatBalance =
                    Report.fiatBalanceOfEntity (Money.bankTime 0) business |> String.fromFloat

                newFiatBalance =
                    Report.fiatBalanceOfEntity (Money.bankTime 0) newBusiness |> String.fromFloat

                logString =
                    "Biz " ++ Entity.getName newBusiness ++ " buy " ++ String.fromInt a ++ ", bal: " ++ oldFiatBalance ++ " >> " ++ newFiatBalance
            in
            { state | seed = seed3, businesses = newBusinesses, log = logItem state logString }


businessBuyGoods1 : State -> State
businessBuyGoods1 state =
    let
        lowInventoryBusiness =
            List.filter
                (\e -> Entity.inventoryAmount "AA" e < state.config.minimumBusinessInventoryOfA)
                state.businesses
    in
    case List.head lowInventoryBusiness of
        Nothing ->
            state

        Just business ->
            let
                maxRandInt =
                    1000

                ( i, newSeed ) =
                    Random.step (Random.int 0 maxRandInt) state.seed

                randomPurchaseAmount : Int -> Int
                randomPurchaseAmount ri =
                    let
                        p =
                            toFloat ri / toFloat maxRandInt

                        range =
                            toFloat (state.config.maximumPurchaseOfA - state.config.minimumPurchaseOfA)
                    in
                    state.config.minimumPurchaseOfA + round (p * range)

                a =
                    randomPurchaseAmount i

                aCC =
                    config.maximumCCRatio * toFloat a |> round

                aFiat =
                    a - aCC

                item =
                    ModelTypes.setQuantity a state.config.itemA

                config =
                    state.config

                oldFiatBalance =
                    Report.fiatBalanceOfEntity (Money.bankTime 0) business |> String.fromFloat

                newBusiness =
                    -- subtract total cost of items purchased from supplier
                    Entity.addToInventory item business
                        |> AH.creditEntity config state.tick config.fiatCurrency Infinite (toFloat aFiat * -config.itemCost)
                        |> AH.creditEntity config state.tick config.complementaryCurrency config.complementaryCurrencyExpiration (toFloat aCC * -config.itemCost)

                newBusinesses =
                    List.Extra.updateIf
                        (\b -> Entity.getName b == Entity.getName newBusiness)
                        (\b -> newBusiness)
                        state.businesses

                newFiatBalance =
                    Report.fiatBalanceOfEntity (Money.bankTime 0) newBusiness |> String.fromFloat

                logString =
                    "Buy " ++ Entity.getName newBusiness ++ ", " ++ String.fromInt a ++ ", FB: " ++ oldFiatBalance ++ " >> " ++ newFiatBalance
            in
            { state | seed = newSeed, businesses = newBusinesses, log = logItem state logString }



--fiatBalance : Entity -> String
--fiatBalance e =
--    String.fromFloat (Report.fiatBalanceOfEntityList (Money.bankTime 0) e)


logItem : State -> String -> List String
logItem state item =
    (String.padRight 3 ' ' (String.fromInt state.tick) ++ ": " ++ item) :: state.log


{-| Choose a low-inventory household at random and buy goods
-}
householdBuyGoods : Int -> State -> State
householdBuyGoods t state =
    let
        sortByAccountValue : Entity -> Int
        sortByAccountValue e =
            Entity.getFiatAccount e
                |> Account.value (Money.bankTime t)
                |> Value.intValue
                |> (\v -> -v)

        orderedHouseholds =
            List.sortBy (\e -> sortByAccountValue e) state.households
                |> List.take 5

        n =
            List.length orderedHouseholds - 1

        ( i, newSeed ) =
            Random.step (Random.int 0 n) state.seed
    in
    case List.Extra.getAt 0 orderedHouseholds of
        Nothing ->
            { state | seed = newSeed }

        Just e ->
            householdBuyGoods_ t e { state | seed = newSeed }


type Step state a
    = Done a
    | Loop state


updateBusinessLog : Entity -> List BusinessLog -> List BusinessLog
updateBusinessLog business log =
    let
        updater bLog =
            if Entity.getName business == bLog.name then
                { bLog | lostSales = bLog.lostSales + 1 }

            else
                bLog
    in
    List.map updater log


nextState : SState -> Step SState ( Maybe Entity, List BusinessLog )
nextState st =
    case List.head st.shops of
        Nothing ->
            Done ( Nothing, st.log )

        Just shop ->
            case Entity.inventoryAmount "AA" shop == 0 of
                True ->
                    Loop { shops = List.drop 1 st.shops, log = updateBusinessLog shop st.log }

                False ->
                    Done ( Just shop, st.log )


type alias SState =
    { shops : List Entity, log : List BusinessLog }


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b


findShopWithPositiveInventory : State -> Entity -> ( Maybe Entity, List BusinessLog )
findShopWithPositiveInventory state household =
    loop { shops = state.businesses, log = state.businessLog } nextState


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


householdBuyGoods_ : Int -> Entity -> State -> State
householdBuyGoods_ t household_ state =
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


householdBuyGoods1 : Int -> State -> State
householdBuyGoods1 t state =
    if List.member (modBy 30 t) state.config.householdPurchaseDays then
        let
            addInventoryOfA : Inventory -> Inventory
            addInventoryOfA inventory =
                Inventory.add state.config.itemA inventory

            addInventoryOfHouseHold : Entity -> Entity
            addInventoryOfHouseHold e =
                Entity.mapInventory addInventoryOfA e

            newHouseholds =
                List.map addInventoryOfHouseHold state.households
        in
        { state | households = newHouseholds }

    else
        state


recordData : Int -> State -> State
recordData tick state =
    let
        i =
            Report.businessInventoryOf "AA" state |> List.head |> Maybe.withDefault 0 |> toFloat

        t =
            toFloat tick
    in
    { state | data = ( t, i ) :: state.data }



-- HELPERS --
