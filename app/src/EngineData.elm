module EngineData exposing
    ( CCEarnings(..)
    , Config
    , business1
    , business2
    , businesses
    , config1
    , config2
    , configurationList
    , educator1
    , educators
    , generateHouseholds
    , initialHousehold
    , supplier1
    , suppliers
    )

import Account exposing (Account)
import CellGrid exposing (Position)
import Color exposing (Color)
import Dict exposing (Dict)
import Entity
    exposing
        ( Characteristics(..)
        , Entity(..)
        , TEntity(..)
        , setColor
        , setName
        , setPosition
        )
import Internal.Types exposing (Expiration(..))
import List.Extra
import ModelTypes exposing (Item(..))
import Money exposing (Money, Value)
import Random



--getConfiguration : Int -> Config
--getConfiguration k =
--    List.Extra.getAt k configurations |> Maybe.withDefault config1


configurationDict : Dict Int Config
configurationDict =
    Dict.fromList [ ( 1, config1 ), ( 2, config2 ) ]


type alias Config =
    { title : String
    , subtitle : String
    , tickLoopInterval : Float
    , cycleLength : Int
    , renderWidth : Float
    , gridWidth : Int

    -- Financial
    , fiatCurrency : Money.Currency
    , fiatCurrencyName : String
    , complementaryCurrency : Money.Currency
    , complementaryCurrencyExpiration : Expiration
    , ccEarnings : CCEarnings

    -- Businesses
    , contentReleaseInterval : Int
    , numberOfTimesToWatchContent : Int
    , businessRadius : Float
    , itemPrice : Money.Value
    , itemCost : Float
    , itemA : Item
    , itemAMoney : Money
    , randomPurchaseFraction : Float
    , minimumBusinessInventoryOfA : Int
    , minimumPurchaseOfA : Int
    , maximumPurchaseOfA : Int
    , maxInventory : Int
    , educationalContentCycle : Int
    , educationPaymentPerCycle : Float
    , maximumCCRatio : Float
    , probabilityOfPurchasing : Float
    , monthlyPurchaseCeilingInUnits : Int
    , monthlyPurchaseCeilingHeadRoom : Int
    , businessRent : Float
    , rentDueDate : Int

    -- Households
    , numberOfHouseholds : Int
    , monthlyItemConsumption : Int
    , householdPurchaseDays : List Int
    , householdMinimumPurchaseAmount : Int
    , householdMaximumPurchaseAmount : Int
    , householdLowInventoryThreshold : Int
    , householdConsumptionDays : List Int
    , householdPayDays : List Int
    , periodicHouseHoldFiatIncome : Float
    , monthlyCCIncome : Money.Value
    }


type CCEarnings
    = CCEarningsON
    | CCEarningsOFF


configurations : List Config
configurations =
    [ config1, config2 ]


config1 : Config
config1 =
    { title = "CASE 1. Fiat currency only"
    , subtitle = "Simplistic random re-order model"
    , tickLoopInterval = 0.3 * 10
    , cycleLength = 360
    , renderWidth = 573
    , gridWidth = 30
    , ccEarnings = CCEarningsOFF

    -- Financial
    , fiatCurrency = fiatCurrency
    , fiatCurrencyName = "Real"
    , complementaryCurrency = cambiatus
    , complementaryCurrencyExpiration = Finite (Money.bankTime 360)
    , educationPaymentPerCycle = 20.0

    -- Businesses
    , contentReleaseInterval = 15
    , numberOfTimesToWatchContent = 1
    , businessRadius = 10.0
    , itemPrice = Money.createValue fiatCurrency 2
    , itemCost = 1.0
    , itemA = Item { name = "AA", price = Money.createValue fiatCurrency 2.0, quantity = 1 }
    , itemAMoney = Money.createInfinite fiatCurrency 0 2.0
    , randomPurchaseFraction = 0.1
    , maxInventory = 20
    , minimumBusinessInventoryOfA = 5
    , minimumPurchaseOfA = 2
    , maximumPurchaseOfA = 15
    , educationalContentCycle = 30
    , maximumCCRatio = 0.0
    , probabilityOfPurchasing = 0.6
    , monthlyPurchaseCeilingInUnits = 70
    , monthlyPurchaseCeilingHeadRoom = 10
    , businessRent = 80
    , rentDueDate = 10

    -- Educators
    -- Households
    , numberOfHouseholds = 20
    , monthlyItemConsumption = 8
    , householdPurchaseDays = [ 1, 5, 9, 13, 17, 21, 25, 28 ] -- not used
    , householdMinimumPurchaseAmount = 2
    , householdMaximumPurchaseAmount = 5
    , householdLowInventoryThreshold = 2
    , householdConsumptionDays = [ 3, 7, 12, 15, 19, 23, 26, 29 ]
    , householdPayDays = [ 1, 15 ]
    , periodicHouseHoldFiatIncome = 8.0
    , monthlyCCIncome = Money.createValue cambiatus 0
    }


config2 : Config
config2 =
    { config1
        | title = "CASE 2: with CC Earnings"
        , subtitle = "Somewhat more interesting"
        , ccEarnings = CCEarningsON
        , maximumCCRatio = 0.15
    }


configurationList =
    [ config1, config2 ]


cambiatus =
    Money.createCompCurrency "Cambiatus"


fiatCurrency =
    Money.createFiatCurrency "Real"


supplier1 : Config -> Entity
supplier1 config =
    Entity
        { name = "S1"
        , entityType = TSupplier
        , complementaryAccount = Account.empty cambiatus
        , fiatAccount = Account.empty fiatCurrency
        , inventory = []
        , position = Position 1 1
        , color = Color.rgba 1.0 0.3 1.0 0.8
        }
        (SupplierCharacteristics {})


educator1 : Config -> Entity
educator1 config =
    Entity
        { name = "E"
        , entityType = TEducator
        , complementaryAccount = Account.empty cambiatus
        , fiatAccount = Account.empty fiatCurrency
        , inventory = []
        , position = Position (config.gridWidth - 1) (config.gridWidth - 1)
        , color = Color.rgba 0.2 0.8 0.2 0.8
        }
        (EducatorCharacteristics {})


business1 : Config -> Entity
business1 config =
    Entity
        { name = "A"
        , entityType = TShop
        , complementaryAccount = Account.empty cambiatus
        , fiatAccount = Account.empty fiatCurrency
        , inventory = [ ModelTypes.setQuantity 20 config.itemA ]
        , position = Position 8 (config.gridWidth - 5)
        , color = Color.rgba 0.4 0.4 1.0 0.8
        }
        (BusinessCharacteristics { radius = config.businessRadius, unitsPurchased = 0 })


business2 config =
    business1 config
        |> setName "B"
        |> setPosition (config.gridWidth - 5) 5


businesses config =
    [ business1 config, business2 config ]


suppliers config =
    [ supplier1 config ]


educators config =
    [ educator1 config ]


initialHousehold : Config -> Entity
initialHousehold config =
    Entity
        { name = "1"
        , entityType = THousehold
        , complementaryAccount = Account.empty cambiatus
        , fiatAccount = Account.empty fiatCurrency |> Account.credit (Money.bankTime 0) (Money.createInfinite fiatCurrency 0 4)
        , inventory = [ ModelTypes.setQuantity 4 config.itemA ]
        , position = Position 20 15
        , color = Color.rgba 0.8 0.6 0.0 0.8
        }
        (HouseholdCharacteristics { monthlyConsumptionA = 0 })


type alias HouseHoldGeneratorState =
    { count : Int
    , seed : Random.Seed
    , existingPositions : List CellGrid.Position
    , households : List Entity
    , maxHouseHolds : Int
    }


initialHouseHoldGeneratorState : Config -> Int -> Int -> HouseHoldGeneratorState
initialHouseHoldGeneratorState config k maxHouseHolds =
    let
        ( i, seed1 ) =
            Random.step (Random.int 0 config.gridWidth) (Random.initialSeed k)

        ( j, seed2 ) =
            Random.step (Random.int 0 config.gridWidth) seed1

        existingPositions =
            Entity.getPosition (initialHousehold config) :: List.map Entity.getPosition (businesses config)
    in
    { count = 1
    , seed = seed2
    , existingPositions = existingPositions
    , households = []
    , maxHouseHolds = maxHouseHolds
    }


generateHouseholds : Config -> Int -> Int -> List Entity
generateHouseholds config intSeed numberOfHouseholds =
    let
        i =
            initialHouseHoldGeneratorState config intSeed config.numberOfHouseholds

        s =
            List.foldl (newState config) i (List.range 0 (numberOfHouseholds - 1))
    in
    s.households


initialInventory config =
    [ config.itemA |> ModelTypes.setQuantity 1 ]


newState : Config -> Int -> HouseHoldGeneratorState -> HouseHoldGeneratorState
newState config k s =
    let
        ( i, seed1 ) =
            Random.step (Random.int 0 config.gridWidth) s.seed

        ( j, seed2 ) =
            Random.step (Random.int 0 config.gridWidth) seed1

        newCount =
            s.count + 1

        newName =
            String.fromInt newCount

        -- thus each household has a unique identifier
        newHousehold =
            initialHousehold config |> setPosition i j |> setName newName |> Entity.setInventory (initialInventory config)
    in
    { s
        | count = newCount
        , seed = seed2
        , existingPositions = Position i j :: s.existingPositions
        , households = newHousehold :: s.households
    }
