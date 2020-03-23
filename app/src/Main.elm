module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import CellGrid.Render
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Engine
import EngineData
import Entity
import Html exposing (Html)
import Http
import List.Extra
import Money
import Report
import SimpleGraph exposing (Option(..))
import State exposing (BusinessLog, State)
import Statistics
import String.Interpolate exposing (interpolate)
import Style
import Time
import Widget.TextField as TextField


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : String
    , output : String
    , counter : Int
    , state : State
    , configurationString : String
    , configurationList : List EngineData.Config
    , configurationIndex : Int
    , configuration : EngineData.Config
    , runState : RunState
    , filterString : String
    , cycleLengthString : String
    , tickRateString : String
    , ccRatioString : String
    , rentString : String
    , runMode : RunMode
    , batchJobState : BatchJobState
    , trialsToRun : Int
    , trialsToRunString : String
    , randomAtmosphericInt : Maybe Int
    , data : List Int -- record of total lost sales per trial
    }


type RunState
    = Running
    | BatchDone
    | Paused
    | End


type RunMode
    = Single
    | Batch


type BatchJobState
    = NoBatch
    | EndBatch
    | InTrial Int



-- MSG


type Msg
    = NoOp
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix
    | CycleRun
    | CycleRunMode
    | Reset
    | AcceptFilter String
    | AcceptCycleLength String
    | AcceptTickRate String
    | AcceptCCRatio String
    | AcceptConfiguration String
    | AcceptRentString String
    | AcceptTrialsString String
    | IncrementModel
    | DecrementModel
    | SetModel Int
    | GotAtmosphericRandomNumber (Result Http.Error String)


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        config : EngineData.Config
        config =
            List.Extra.getAt 0 EngineData.configurationList |> Maybe.withDefault EngineData.config1
    in
    ( { input = "App started"
      , output = "App started"
      , counter = 0
      , configurationString = "1"
      , configurationList = EngineData.configurationList
      , configurationIndex = 0
      , configuration = config
      , state = State.configure config 400
      , runState = Paused
      , runMode = Single
      , batchJobState = NoBatch
      , trialsToRun = 5
      , trialsToRunString = "5"
      , filterString = ""
      , ccRatioString = String.fromFloat config.maximumCCRatio
      , randomAtmosphericInt = Nothing
      , data = []
      , cycleLengthString = String.fromInt config.cycleLength
      , tickRateString = String.fromFloat config.tickLoopInterval
      , rentString = String.fromFloat config.businessRent
      }
    , getRandomNumber
    )


changeConfig : Int -> Model -> Model
changeConfig k model =
    let
        configuration : EngineData.Config
        configuration =
            List.Extra.getAt k EngineData.configurationList
                |> Maybe.withDefault EngineData.config1

        -- |> updateParametersInConfig model
        seed =
            case model.randomAtmosphericInt of
                Nothing ->
                    400

                Just s ->
                    s

        newState =
            State.configure configuration seed
    in
    { model
        | configurationString = String.fromInt k
        , configurationIndex = k
        , configuration = configuration
        , state = newState
    }
        |> updateParameters


subscriptions model =
    Time.every model.state.config.tickLoopInterval Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CellGrid msg_ ->
            ( model, Cmd.none )

        Tick _ ->
            case model.runState of
                Running ->
                    let
                        ( counter, runState, batchJobState_ ) =
                            Debug.log "(co,  rs, bjs)"
                                (updateRunParameters model)

                        newData =
                            updateData model runState batchJobState_

                        newState =
                            model.state
                                |> (\st -> { st | businessLog = updateBusinessLog model model.runMode counter st.businessLog })
                                |> Engine.nextState model.configuration counter

                        runState2 =
                            Debug.log "runState2"
                                (case model.runMode of
                                    Single ->
                                        runState

                                    Batch ->
                                        case batchJobState_ of
                                            InTrial k ->
                                                if runState == BatchDone && k > model.trialsToRun - 1 then
                                                    End

                                                else
                                                    Running

                                            _ ->
                                                End
                                )
                    in
                    ( { model
                        | counter = counter
                        , state = newState
                        , runState = runState2
                        , data = newData
                        , batchJobState = batchJobState_
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CycleRun ->
            case model.runState of
                Paused ->
                    ( { model | runState = Running }, Cmd.none )

                Running ->
                    ( { model | runState = Paused }, Cmd.none )

                BatchDone ->
                    ( { model | runState = Running }, Cmd.none )

                End ->
                    { model
                        | runState = Running
                        , counter = 0

                        -- , data = []
                    }
                        |> withCmd getRandomNumber

        CycleRunMode ->
            case model.runMode of
                Single ->
                    { model | runMode = Batch, batchJobState = InTrial 0 } |> withNoCmd

                Batch ->
                    { model | runMode = Single, batchJobState = NoBatch } |> withNoCmd

        Reset ->
            let
                config =
                    model.configuration
            in
            ( { model
                | state = State.configure config (model.randomAtmosphericInt |> Maybe.withDefault 400)
                , runState = Paused
                , counter = 0
                , data = []
              }
                |> updateCCRatio model.ccRatioString
                |> updateCycleLength model.cycleLengthString
                |> updateRent model.rentString
                |> updateTickRate model.tickRateString
                |> updateTrialsToRun model.trialsToRunString
            , getRandomNumber
            )

        AcceptFilter str ->
            ( { model | filterString = str }, Cmd.none )

        AcceptCycleLength str ->
            updateCycleLength str model |> withNoCmd

        AcceptTickRate str ->
            updateTickRate str model |> withNoCmd

        AcceptCCRatio str ->
            updateCCRatio str model |> withNoCmd

        AcceptRentString str ->
            updateRent str model |> withNoCmd

        AcceptTrialsString str ->
            updateTrialsToRun str model |> withNoCmd

        AcceptConfiguration str ->
            case String.toInt str of
                Nothing ->
                    ( { model | configurationString = str }, Cmd.none )

                Just k ->
                    let
                        index =
                            k - 1
                    in
                    model
                        |> changeConfig index
                        |> withNoCmd

        IncrementModel ->
            let
                n =
                    List.length model.configurationList

                index =
                    modBy n (model.configurationIndex + 1)
            in
            model
                |> changeConfig index
                |> withNoCmd

        SetModel k ->
            model
                |> changeConfig k
                |> withNoCmd

        DecrementModel ->
            let
                n =
                    List.length model.configurationList

                index =
                    modBy n (model.configurationIndex - 1)
            in
            model
                |> changeConfig index
                |> withNoCmd

        GotAtmosphericRandomNumber result ->
            case result of
                Ok str ->
                    case String.toInt (String.trim str) of
                        Nothing ->
                            ( model, Cmd.none )

                        Just rn ->
                            let
                                config : EngineData.Config
                                config =
                                    updateParametersInConfig model model.state.config

                                newState =
                                    State.configure config (model.randomAtmosphericInt |> Maybe.withDefault 400)
                            in
                            { model
                                | randomAtmosphericInt = Just rn
                                , state = State.configure config rn |> (\state -> { state | data = [] })
                            }
                                |> withNoCmd

                Err _ ->
                    ( model, Cmd.none )



-- HELPER


updateData : Model -> RunState -> BatchJobState -> List Int
updateData model runState bjs =
    case model.runMode of
        Single ->
            case runState of
                End ->
                    State.lostSales model.state.businessLog :: model.data

                _ ->
                    model.data

        Batch ->
            case ( runState, bjs ) of
                ( BatchDone, InTrial k ) ->
                    if k < model.trialsToRun then
                        State.lostSales model.state.businessLog :: model.data

                    else
                        model.data

                ( BatchDone, EndBatch ) ->
                    model.data

                _ ->
                    model.data


updateRunParameters : Model -> ( Int, RunState, BatchJobState )
updateRunParameters model =
    case model.runMode of
        Single ->
            case model.counter < model.state.config.cycleLength of
                True ->
                    ( model.counter + 1, Running, NoBatch )

                False ->
                    ( 0, End, NoBatch )

        Batch ->
            let
                n =
                    model.trialsToRun - 1
            in
            case model.batchJobState of
                InTrial k ->
                    case ( model.counter < model.state.config.cycleLength, k <= n + 1 ) of
                        ( True, _ ) ->
                            ( model.counter + 1, Running, InTrial k )

                        ( False, True ) ->
                            ( 0, BatchDone, InTrial (k + 1) )

                        ( False, False ) ->
                            ( 0, End, EndBatch )

                _ ->
                    ( 0, End, EndBatch )


updateBusinessLog : Model -> RunMode -> Int -> List BusinessLog -> List BusinessLog
updateBusinessLog model runMode_ counter_ log =
    case ( runMode_, counter_ ) of
        ( Batch, 0 ) ->
            State.newBusinessLog model.state

        _ ->
            log


updateParametersInConfig : Model -> EngineData.Config -> EngineData.Config
updateParametersInConfig model configuration =
    configuration
        |> updateCycleLengthInConfig (String.fromInt model.state.config.cycleLength)
        |> updateTickRateInConfig (String.fromFloat model.state.config.tickLoopInterval)
        |> updateCCRatioInConfig (String.fromFloat model.state.config.maximumCCRatio)
        |> updateRentInConfig (String.fromFloat model.state.config.businessRent)


updateParameters : Model -> Model
updateParameters model =
    model
        |> updateCycleLength (String.fromInt model.state.config.cycleLength)
        |> updateTickRate (String.fromFloat model.state.config.tickLoopInterval)
        |> updateCCRatio (String.fromFloat model.state.config.maximumCCRatio)
        |> updateRent (String.fromFloat model.state.config.businessRent)
        |> updateTrialsToRun (String.fromFloat model.state.config.businessRent)


updateCycleLengthInConfig : String -> EngineData.Config -> EngineData.Config
updateCycleLengthInConfig str config =
    case String.toInt str of
        Nothing ->
            config

        Just cycleLength_ ->
            { config | cycleLength = cycleLength_ }


updateCycleLength : String -> Model -> Model
updateCycleLength str model =
    let
        newConfig =
            updateCycleLengthInConfig str model.state.config

        newState =
            State.updateConfig newConfig model.state
    in
    { model | cycleLengthString = str, state = newState }


updateTickRateInConfig : String -> EngineData.Config -> EngineData.Config
updateTickRateInConfig str config =
    case String.toFloat str of
        Nothing ->
            config

        Just tickRate_ ->
            { config | tickLoopInterval = tickRate_ }


updateTickRate : String -> Model -> Model
updateTickRate str model =
    let
        newConfig =
            updateTickRateInConfig str model.state.config

        newState =
            State.updateConfig newConfig model.state
    in
    { model | tickRateString = str, state = newState }


updateCCRatioInConfig : String -> EngineData.Config -> EngineData.Config
updateCCRatioInConfig str config =
    case String.toFloat str of
        Nothing ->
            config

        Just ccRatio_ ->
            { config | maximumCCRatio = Debug.log "ccRatio_" ccRatio_ }


updateCCRatio : String -> Model -> Model
updateCCRatio str model =
    let
        _ =
            Debug.log "updateCCRatio" str

        newConfig =
            updateCCRatioInConfig str model.state.config

        newState =
            State.updateConfig newConfig model.state
    in
    { model | ccRatioString = Debug.log "CCR@@" str, state = newState }


updateRentInConfig : String -> EngineData.Config -> EngineData.Config
updateRentInConfig str config =
    case String.toFloat str of
        Nothing ->
            config

        Just rent_ ->
            { config | businessRent = rent_ }


updateRent : String -> Model -> Model
updateRent str model =
    let
        newConfig =
            updateRentInConfig str model.state.config

        newState =
            State.updateConfig newConfig model.state
    in
    { model | rentString = str, state = newState }


updateTrialsToRun : String -> Model -> Model
updateTrialsToRun str model =
    case String.toInt str of
        Nothing ->
            { model | trialsToRunString = str }

        Just k ->
            { model | trialsToRunString = str, trialsToRun = k }



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ centerX ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column Style.mainColumn
        [ title "Simulator II"
        , row [ centerX, spacing 5 ]
            [ displayState model
            , displayLog model
            , dashboard model
            , controlPanel model
            ]
        , graphDisplay model
        , footer model
        ]


lineGraphAttributes =
    { graphHeight = 35
    , graphWidth = 1200
    , options = [ Color "blue", XTickmarks 12, YTickmarks 0, DeltaX 3 ]
    }


bgColor model =
    case model.runState of
        End ->
            Background.color Style.endColor

        Paused ->
            Background.color Style.pausedColor

        Running ->
            Background.color Style.lightColor

        BatchDone ->
            Background.color Style.lightBlue


graphDisplay model =
    row [ paddingEach { top = 20, bottom = 0, left = 0, right = 0 }, moveDown 10, width fill, height (px 50), bgColor model ]
        [ SimpleGraph.barChart lineGraphAttributes (List.map Tuple.second model.state.data |> List.reverse) |> Element.html ]


controlPanel : Model -> Element Msg
controlPanel model =
    column Style.controlPanel
        [ el [] (text <| "Control Panel")
        , el [ paddingXY 0 2 ] (text <| "")
        , cycleLengthInput model
        , trialsToRunInput model
        , tickRateInput model
        , ccRatioInput model
        , rentInput model
        ]


dashboard : Model -> Element msg
dashboard model =
    column Style.dashboard
        [ el [] (text <| model.state.config.title)
        , el [] (text <| model.state.config.subtitle)
        , el [] (text <| "------------------------------")
        , el [] (text <| "Cycle length = " ++ String.fromInt model.state.config.cycleLength)
        , el [] (text <| "Rate = " ++ String.fromFloat model.state.config.tickLoopInterval)
        , el [] (text <| clock model.counter)
        , el [] (text <| "------------------------------")
        , el [] (text <| "Households = " ++ String.fromInt (model.state.households |> List.length))
        , el [] (text <| "Household acct bal = " ++ fiatHoldingsDisplay model)
        , el [] (text <| "Household inventory = " ++ String.fromInt (Report.householdInventoryOf "AA" model.state))
        , el [] (text <| "[min, max] = " ++ (List.map String.fromInt (Report.minMaxHouseholdInventoryOf "AA" model.state) |> String.join ", "))
        , el [] (text <| "Total purchases = " ++ String.fromInt model.state.totalHouseholdPurchases)
        , el [] (text <| "Total consumed = " ++ String.fromInt model.state.totalHouseholdConsumption)
        , el [] (text <| "Net purchases = " ++ String.fromInt (model.state.totalHouseholdPurchases - model.state.totalHouseholdConsumption))
        , el [] (text <| "------------------------------")
        , el [] (text <| "Business inventories = " ++ businessInventory model)
        , el [] (text <| "Fiat balances = " ++ fiatBalances model)
        , el [] (text <| "CC balances = " ++ ccBalances model)
        , el [] (text <| "CC Ratio = " ++ String.fromFloat model.state.config.maximumCCRatio)
        , el [] (text <| "------------------------------")
        , row [ spacing 8 ] (displayLostSales model ++ [ totalLostSales model ])
        , displayStatistics model
        , el [] (text <| Debug.toString model.data)
        ]


displayStatistics model =
    let
        stats =
            Statistics.stats (List.map toFloat model.data)
    in
    row [ spacing 8 ]
        [ el [] (text <| String.fromInt stats.n)
        , el [] (text <| String.fromFloat stats.mean)
        , el [] (text <| String.fromFloat stats.stdev)
        ]


totalLostSales : Model -> Element msg
totalLostSales model =
    let
        data =
            model.state.businessLog

        total =
            List.sum (List.map .lostSales data)
    in
    el [] (text <| "Total: " ++ String.fromInt total)


displayLostSales : Model -> List (Element msg)
displayLostSales model =
    let
        data =
            model.state.businessLog

        display : BusinessLog -> Element msg
        display bl =
            row [ spacing 2 ]
                [ el [ width (px 25) ] (text <| bl.name ++ ":")
                , el [ width (px 25) ] (text <| String.fromInt bl.lostSales)
                ]
    in
    List.map display data


displayLog : Model -> Element msg
displayLog model =
    let
        displayItem : String -> Element msg
        displayItem str =
            el [] (text str)

        filteredLog =
            case model.filterString == "" of
                True ->
                    model.state.log

                False ->
                    let
                        badStrings =
                            String.words model.filterString |> List.map String.trim

                        filter =
                            \item -> conjunctiveFilter (\target item_ -> String.contains item_ target) badStrings item
                    in
                    List.filter filter model.state.log

        log =
            "Log" :: "--------------------------" :: filteredLog
    in
    List.map displayItem log
        |> column Style.log


{-|

    f = \target item -> target == item
    --> disjunctiveFilter f ["a", "b"] "a"

    disjunctiveFilter f ["a", "b"] "a"
    --> True : Bool

    disjunctiveFilter f ["a", "b"] "b"
    --> True : Bool

    disjunctiveFilter f ["a", "b"] "c"
    --> False

-}
disjunctiveFilter : (target -> item -> Bool) -> List item -> target -> Bool
disjunctiveFilter filter list target =
    List.map (filter target) list
        |> booleanOr


conjunctiveFilter : (target -> item -> Bool) -> List item -> target -> Bool
conjunctiveFilter filter list target =
    not (disjunctiveFilter filter list target)


booleanOr : List Bool -> Bool
booleanOr list =
    List.foldl (||) False list


businessInventory : Model -> String
businessInventory model =
    List.map String.fromInt (Report.businessInventoryOf "AA" model.state)
        |> String.join ", "


fiatBalances : Model -> String
fiatBalances model =
    List.map String.fromFloat (Report.fiatBalanceOfEntityList (Money.bankTime model.state.tick) model.state.businesses)
        |> String.join ", "


ccBalances : Model -> String
ccBalances model =
    List.map String.fromFloat (Report.ccBalanceOfEntityList (Money.bankTime model.state.tick) model.state.businesses)
        |> String.join ", "


footer model =
    row
        [ alignBottom
        , paddingXY 10 0
        , Font.size 14
        , spacing 15
        , centerX
        , Background.color Style.lightColor
        , width fill
        , height (px 40)
        ]
        [ runModeButton model
        , resetButton model
        , runButton model
        , el [ Font.family [ Font.typeface "Courier" ] ] (text <| clock model.counter)
        , filterInput model
        , row [ spacing 16, alignLeft ]
            [ -- configurationInput model
              -- incrementModelButton model
              --, decrementModelButton model
              modelButton 0 model
            , modelButton 1 model
            , el [ Font.family [ Font.typeface "Courier" ], width (px 240) ] (text <| model.state.config.title)
            ]

        --, randomSeedDisplay model
        ]


randomSeedDisplay : Model -> Element msg
randomSeedDisplay model =
    let
        n =
            case model.randomAtmosphericInt of
                Nothing ->
                    "400"

                Just k ->
                    String.fromInt k
    in
    el [] (text <| "Seed: " ++ n)


fiatHoldingsDisplay model =
    case Entity.fiatHoldingsOEntities model.counter model.state.households of
        Just value ->
            Money.valueToString value

        Nothing ->
            "--"


clock : Int -> String
clock k =
    let
        day =
            k |> String.fromInt |> String.padLeft 0 ' '

        month =
            k // 30 |> (\x -> x + 1) |> String.fromInt |> String.padLeft 2 '0'

        dayInMonth =
            modBy 30 k |> String.fromInt |> String.padLeft 2 '0'
    in
    interpolate "t = {0}: {1}/{2}" [ day, dayInMonth, month ]


displayState : Model -> Element Msg
displayState model =
    row [ centerX, spacing 10 ]
        [ Engine.render
            model.configuration
            model.state
            |> Element.html
            |> Element.map CellGrid
        ]


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.color Style.titleColor, paddingEach { top = 40, bottom = 0, left = 0, right = 0 } ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]



{- Buttons -}


runButton : Model -> Element Msg
runButton model =
    let
        label =
            case model.runState of
                Running ->
                    "Pause"

                BatchDone ->
                    "Pause"

                Paused ->
                    "Run"

                End ->
                    "Start"
    in
    row []
        [ Input.button Style.button
            { onPress = Just CycleRun
            , label = el [ centerY ] (text label)
            }
        ]


runModeButton : Model -> Element Msg
runModeButton model =
    let
        label =
            case model.runMode of
                Single ->
                    "Single"

                Batch ->
                    "Batch"
    in
    row []
        [ Input.button Style.button
            { onPress = Just CycleRunMode
            , label = el [ centerY ] (text label)
            }
        ]


resetButton model =
    row []
        [ Input.button Style.button
            { onPress = Just Reset
            , label = el [ centerY ] (text "Reset")
            }
        ]


filterInput model =
    Input.text [ width (px 200), height (px 30), paddingEach { top = 8, bottom = 0, left = 4, right = 0 } ]
        { onChange = AcceptFilter
        , text = model.filterString
        , placeholder = Nothing
        , label = Input.labelLeft [ centerY ] (text <| "Exclude from log: ")
        }


dashboardInput msg text label =
    TextField.make msg text label
        |> TextField.withHeight 30
        |> TextField.withWidth 50
        |> TextField.withLabelWidth 70
        |> TextField.toElement


cycleLengthInput model =
    dashboardInput AcceptCycleLength model.cycleLengthString "Cycle"


tickRateInput model =
    dashboardInput AcceptTickRate model.tickRateString "Rate"


ccRatioInput model =
    dashboardInput AcceptCCRatio model.ccRatioString "CC Ratio"


rentInput model =
    dashboardInput AcceptRentString model.rentString "Rent"


trialsToRunInput model =
    dashboardInput AcceptTrialsString model.trialsToRunString "Trials"


incrementModelButton model =
    row []
        [ Input.button Style.button
            { onPress = Just IncrementModel
            , label = el [ centerY ] (text "+")
            }
        ]


decrementModelButton model =
    row []
        [ Input.button Style.button
            { onPress = Just DecrementModel
            , label = el [ centerY ] (text "-")
            }
        ]


modelButton k model =
    let
        buttonStyle_ =
            case model.configurationIndex == k of
                True ->
                    Style.selectedButton

                False ->
                    Style.button
    in
    row []
        [ Input.button buttonStyle_
            { onPress = Just (SetModel k)
            , label = el [ centerY ] (text <| "Model " ++ String.fromInt (k + 1))
            }
        ]


configurationInput model =
    Input.text [ width (px 25), height (px 30), paddingEach { top = 8, bottom = 0, left = 4, right = 0 } ]
        { onChange = AcceptConfiguration
        , text = model.configurationString
        , placeholder = Nothing
        , label = Input.labelLeft [ centerY ] (text <| "Model: ")
        }


getRandomNumber : Cmd Msg
getRandomNumber =
    Http.get
        { url = randomNumberUrl 9
        , expect = Http.expectString GotAtmosphericRandomNumber
        }


randomNumberUrl : Int -> String
randomNumberUrl maxDigits =
    let
        maxNumber =
            10 ^ maxDigits

        prefix =
            "https://www.random.org/integers/?num=1&min=1&max="

        suffix =
            "&col=1&base=10&format=plain&rnd=new"
    in
    prefix ++ String.fromInt maxNumber ++ suffix
