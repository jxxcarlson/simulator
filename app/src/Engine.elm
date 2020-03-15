module Engine exposing (nextState, render)

import Action
import CellGrid exposing (CellGrid, Dimensions)
import CellGrid.Canvas exposing (CellStyle)
import Color exposing (Color)
import EngineData exposing (Config)
import Entity exposing (Entity, TEntity(..))
import Html exposing (Html)
import Random
import State exposing (State, initialState)
import Utility


config =
    { maxRandInt = 100000 }


render : Config -> State -> Html CellGrid.Canvas.Msg
render config_ s =
    CellGrid.Canvas.asHtml { width = round config_.renderWidth, height = round config_.renderWidth }
        (cellStyle config_)
        (s.households ++ s.businesses ++ s.suppliers ++ s.educators)


entityRadius : Entity -> Float
entityRadius e =
    case Entity.getType e of
        THousehold ->
            1.5 * toFloat (1 + Entity.inventoryAmount "AA" e)

        TShop ->
            6 + 0.5 * toFloat (1 + Entity.inventoryAmount "AA" e)

        TSupplier ->
            15

        TEducator ->
            15


cellStyle : Config -> CellStyle Entity
cellStyle config_ =
    { toColor = \e -> Entity.getColor e
    , toRadius = entityRadius
    , toPosition = \e -> Entity.getPosition e
    , cellWidth = config_.renderWidth / toFloat config_.gridWidth
    , cellHeight = config_.renderWidth / toFloat config_.gridWidth
    }


toCellGrid : Config -> State -> CellGrid Color
toCellGrid config_ s =
    let
        gridWidth =
            config_.gridWidth

        initialGrid : CellGrid Color
        initialGrid =
            CellGrid.initialize (Dimensions gridWidth gridWidth) (\i j -> Color.black)

        setCell : Entity -> CellGrid Color -> CellGrid Color
        setCell e grid =
            CellGrid.set (Entity.getPosition e) (Entity.getColor e) grid
    in
    List.foldl setCell initialGrid (s.businesses ++ s.households)


newRandomNumber : State -> State
newRandomNumber state =
    let
        ( newRandInt, newSeed ) =
            Random.step (Random.int 0 config.maxRandInt) state.seed
    in
    { state | randInt = newRandInt, seed = newSeed }


nextState : Config -> Int -> State -> State
nextState config_ t state =
    { state | tick = t }
        |> Action.readEducationalContent
        |> Action.businessBuyGoods
        |> Action.payHouseholds config_ t
        |> Utility.iterate 2 (Action.householdBuyGoods t)
        |> Action.consumeA t
        |> Action.recordData t
