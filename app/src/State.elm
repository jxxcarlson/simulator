module State exposing (State, configure, initialState, initialStateWithHouseholdsAndSeed)

import EngineData exposing (Config)
import Entity exposing (Entity)
import Random


type alias State =
    { suppliers : List Entity
    , educators : List Entity
    , businesses : List Entity
    , households : List Entity
    , seed : Random.Seed
    , randInt : Int
    , config : EngineData.Config
    , totalHouseholdPurchases : Int
    , totalHouseholdConsumption : Int
    , log : List String
    , data : List ( Float, Float )
    , tick : Int
    }


initialState : State
initialState =
    { suppliers = []
    , educators = []
    , businesses = []
    , households = []
    , seed = Random.initialSeed 1234
    , randInt = 4321
    , config = EngineData.config1
    , totalHouseholdPurchases = 0
    , totalHouseholdConsumption = 0
    , log = []
    , data = []
    , tick = 0
    }


configure : Config -> Int -> State
configure config seed =
    initialState
        |> configureWithGivenSeed config seed
        |> configureWithHouseholds config seed config.numberOfHouseholds
        |> configureWithBusinesses config
        |> configureWithSuppliers config
        |> configureWithEducators config
        |> (\state -> { state | config = config })


configureWithHouseholds : Config -> Int -> Int -> State -> State
configureWithHouseholds config intSeed numberOfHouseholds state =
    { state | households = EngineData.generateHouseholds config intSeed numberOfHouseholds }


configureWithBusinesses : Config -> State -> State
configureWithBusinesses config state =
    { state | businesses = EngineData.businesses config }


configureWithSuppliers : Config -> State -> State
configureWithSuppliers config state =
    { state | suppliers = EngineData.suppliers config }


configureWithEducators : Config -> State -> State
configureWithEducators config state =
    { state | educators = EngineData.educators config }


configureWithSeed : Config -> Random.Seed -> State -> State
configureWithSeed config seed state =
    let
        ( i, newSeed ) =
            Random.step (Random.int 0 100000) seed
    in
    { state | seed = newSeed }


configureWithGivenSeed : Config -> Int -> State -> State
configureWithGivenSeed config k state =
    let
        newSeed =
            Random.initialSeed k
    in
    { state | seed = newSeed }


initialStateWithHouseholdsAndSeed : Config -> Random.Seed -> Int -> State
initialStateWithHouseholdsAndSeed config seed numberOfHouseholds =
    let
        s =
            initialState

        ( i, newSeed ) =
            Random.step (Random.int 0 100000) seed
    in
    { s
        | households = EngineData.generateHouseholds config i numberOfHouseholds
        , businesses = EngineData.businesses config
        , suppliers = EngineData.suppliers config
        , seed = newSeed
    }
