module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Loc =
    Float


type alias Programmers =
    Int


type alias Cash =
    Int


type alias Model =
    { time : Time.Posix -- time of last tick we updated the model
    , loc : Loc -- lines of code
    , programmers : Int -- programmers to write the code
    , cash : Cash -- cash to hire more programmers
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) 0 0 20, Cmd.none )



-- UPDATE


type Msg
    = IncrementLoc
    | IncrementProgrammers
    | Tick Time.Posix


updateLoc : Loc -> Programmers -> Int -> Loc
updateLoc originalLoc programmers duration =
    originalLoc + toFloat programmers * (toFloat duration / 1000.0)


timeDifference : Time.Posix -> Time.Posix -> Int
timeDifference newTime oldTime =
    Time.posixToMillis newTime - Time.posixToMillis oldTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementLoc ->
            ( { model | loc = model.loc + 1 }, Cmd.none )

        IncrementProgrammers ->
            ( { model | cash = model.cash - 5, programmers = model.programmers + 1 }, Cmd.none )

        Tick newTime ->
            let
                duration =
                    timeDifference newTime model.time

                newLoc =
                    updateLoc model.loc model.programmers duration
            in
            ( { model | time = newTime, loc = newLoc }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 50 Tick



-- VIEW


zone : Time.Zone
zone =
    Time.utc


view : Model -> Html Msg
view { cash, loc, programmers } =
    let
        cashString =
            String.fromInt cash

        locString =
            String.fromInt (floor loc)

        programmersString =
            String.fromInt programmers
    in
    div []
        [ div [] [ text ("Cash on hand: " ++ cashString) ]
        , div [] [ text (locString ++ " lines of code written") ]
        , button [ onClick IncrementLoc ] [ text "Type" ]
        , div [] [ text (programmersString ++ " programmers") ]
        , button [ onClick IncrementProgrammers, disabled (cash < 5) ] [ text "Hire programmer" ]
        ]
