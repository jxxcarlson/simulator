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
import State exposing (State)
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
    , randomAtmosphericInt : Maybe Int
    }


type RunState
    = Running
    | Paused
    | End


type Msg
    = NoOp
    | CellGrid CellGrid.Render.Msg
    | Tick Time.Posix
    | ToggleRun
    | Reset
    | AcceptFilter String
    | AcceptConfiguration String
    | IncrementModel
    | DecrementModel
    | GotAtomsphericRandomNumber (Result Http.Error String)


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
      , filterString = ""
      , randomAtmosphericInt = Nothing
      }
    , getRandomNumber
    )


changeConfig : Int -> Model -> Model
changeConfig k model =
    let
        configuration =
            List.Extra.getAt k EngineData.configurationList |> Maybe.withDefault EngineData.config1

        _ =
            Debug.log "changeConfig, title" configuration.title

        seed =
            case model.randomAtmosphericInt of
                Nothing ->
                    400

                Just s ->
                    s

        state =
            State.configure configuration seed

        _ =
            Debug.log "changeConfig, state, title" state.config.title
    in
    { model
        | configurationString = String.fromInt k
        , configurationIndex = Debug.log "K" k
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
                        ( counter, runState ) =
                            if model.counter >= model.state.config.cycleLength then
                                ( model.counter, End )

                            else
                                ( model.counter + 1, Running )
                    in
                    ( { model
                        | counter = counter
                        , state = Engine.nextState model.configuration counter model.state
                        , runState = runState
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleRun ->
            case model.runState of
                Paused ->
                    ( { model | runState = Running }, Cmd.none )

                Running ->
                    ( { model | runState = Paused }, Cmd.none )

                End ->
                    ( { model | runState = End }, Cmd.none )

        Reset ->
            let
                config =
                    model.configuration
            in
            ( { model
                | state = State.configure config (model.randomAtmosphericInt |> Maybe.withDefault 400)
                , runState = Paused
                , counter = 0
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

        GotAtomsphericRandomNumber result ->
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
        ]


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
                            \item -> conjunctiveFilter (\target item_ -> String.contains (Debug.log "T" item_) target) badStrings item
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
        [ resetButton model
        , runButton model
        , el [ Font.family [ Font.typeface "Courier" ] ] (text <| clock model.counter)
        , filterInput model
        , row [ spacing 5, alignLeft ]
            [ -- configurationInput model
              incrementModelButton model
            , decrementModelButton model
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
                    "Running"

                Paused ->
                    "Run"

                End ->
                    "End"
    in
    row []
        [ Input.button Style.button
            { onPress = Just ToggleRun
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
    Input.text [ width (px 300), height (px 30), paddingEach { top = 8, bottom = 0, left = 4, right = 0 } ]
        { onChange = AcceptFilter
        , text = model.filterString
        , placeholder = Nothing
        , label = Input.labelLeft [ centerY ] (text <| "Exclude words: ")
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
        , expect = Http.expectString GotAtomsphericRandomNumber
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
