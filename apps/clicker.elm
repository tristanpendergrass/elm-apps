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
-- lines of code


type alias Loc =
    Float


type alias Programmers =
    Int


type alias Cash =
    Int


type alias Model =
    { loc : Loc
    , programmers : Int
    , time : Time.Posix
    , cash : Cash
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0 (Time.millisToPosix 0) 20, Cmd.none )



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
subscriptions model =
    Time.every 50 Tick



-- VIEW


zone : Time.Zone
zone =
    Time.utc


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Cash on hand: " ++ String.fromInt model.cash) ]
        , div [] [ text (String.fromInt (floor model.loc) ++ " lines of code written") ]
        , button [ onClick IncrementLoc ] [ text "Type" ]
        , div [] [ text (String.fromInt model.programmers ++ " programmers") ]
        , button [ onClick IncrementProgrammers, disabled (model.cash < 5) ] [ text "Hire programmer" ]
        ]
