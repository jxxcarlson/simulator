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


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
    , runMode : RunMode
    , batchJobState : BatchJobState
    , trialsToRun : Int
    , randomAtmosphericInt : Maybe Int
    , data : List Int
    }


type RunState
    = Running
    | Paused
    | End


type RunMode
    = Single
    | Batch


type BatchJobState
    = NoBatch
    | EndBatch
    | InTrial Int


type Msg
    = NoOp
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix
    | CycleRun
    | CycleRunMode
    | Reset
    | AcceptFilter String
    | AcceptConfiguration String
    | IncrementModel
    | DecrementModel
    | SetModel Int
    | GotAtmosphericRandomNumber (Result Http.Error String)


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "App started"
      , output = "App started"
      , counter = 0
      , configurationString = "1"
      , configurationList = EngineData.configurationList
      , configurationIndex = 0
      , configuration = List.Extra.getAt 0 EngineData.configurationList |> Maybe.withDefault EngineData.config1
      , state = State.configure EngineData.config1 400
      , runState = Paused
      , runMode = Single
      , batchJobState = NoBatch
      , trialsToRun = 5
      , filterString = ""
      , randomAtmosphericInt = Nothing
      , data = []
      }
    , getRandomNumber
    )


changeConfig : Int -> Model -> Model
changeConfig k model =
    let
        configuration =
            List.Extra.getAt k EngineData.configurationList |> Maybe.withDefault EngineData.config1

        seed =
            case model.randomAtmosphericInt of
                Nothing ->
                    400

                Just s ->
                    s

        state =
            State.configure configuration seed
    in
    { model
        | configurationString = String.fromInt k
        , configurationIndex = k
        , configuration = configuration
        , state = State.configure configuration seed
    }


subscriptions model =
    Time.every model.configuration.tickLoopInterval Tick


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
                                            case ( model.counter < model.state.config.cycleLength, k < n ) of
                                                ( True, _ ) ->
                                                    ( model.counter + 1, Running, InTrial k )

                                                ( False, True ) ->
                                                    ( 0, Running, InTrial (k + 1) )

                                                ( False, False ) ->
                                                    ( 0, End, EndBatch )

                                        _ ->
                                            ( 0, End, EndBatch )

                        updateBusinessLog : RunMode -> Int -> List BusinessLog -> List BusinessLog
                        updateBusinessLog runMode_ counter_ log =
                            case ( runMode_, counter_ ) of
                                ( Batch, 0 ) ->
                                    State.newBusinessLog model.state

                                _ ->
                                    log

                        updateData : RunState -> State -> List Int -> List Int
                        updateData runState_ state data_ =
                            case model.runMode of
                                Single ->
                                    case runState of
                                        End ->
                                            State.lostSales state.businessLog :: data_

                                        _ ->
                                            data_

                                Batch ->
                                    case counter of
                                        0 ->
                                            State.lostSales state.businessLog :: data_

                                        _ ->
                                            data_

                        newState =
                            model.state
                                |> (\st -> { st | businessLog = updateBusinessLog model.runMode counter st.businessLog })
                                |> Engine.nextState model.configuration counter
                    in
                    ( { model
                        | counter = counter
                        , state = newState
                        , runState = runState
                        , data = updateData runState model.state model.data
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

                End ->
                    --( { model | runState = End }, Cmd.none )
                    let
                        config =
                            model.configuration
                    in
                    ( { model
                        | state = State.configure config (model.randomAtmosphericInt |> Maybe.withDefault 400)
                        , runState = Running
                        , counter = 0
                      }
                    , getRandomNumber
                    )

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
            , getRandomNumber
            )

        AcceptFilter str ->
            ( { model | filterString = str }, Cmd.none )

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
                            ( { model
                                | randomAtmosphericInt = Just rn
                                , state = State.configure model.configuration rn
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( model, Cmd.none )



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


graphDisplay model =
    row [ paddingEach { top = 20, bottom = 0, left = 0, right = 0 }, moveDown 10, width fill, height (px 50), bgColor model ]
        [ SimpleGraph.barChart lineGraphAttributes (List.map Tuple.second model.state.data |> List.reverse) |> Element.html ]


dashboard : Model -> Element msg
dashboard model =
    column Style.dashboard
        [ el [] (text <| model.state.config.title)
        , el [] (text <| model.state.config.subtitle)
        , el [] (text <| "------------------------------")
        , el [] (text <| "Cycle length = " ++ String.fromInt model.state.config.cycleLength)
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
        , el [] (text <| "------------------------------")
        , row [ spacing 4 ] (displayLostSales model ++ [ totalLostSales model ])
        , displayStatistics model
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
                [ el [ width (px 20) ] (text <| bl.name ++ ":")
                , el [ width (px 20) ] (text <| String.fromInt bl.lostSales)
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
