module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Html, button, div, h1, li, text, ul)
import Html.Events exposing (onClick)
import Random


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Card
    = Zero
    | One
    | MinusOne
    | Two
    | MinusTwo
    | Crit
    | Null


type alias Model =
    { deck : Array Card
    , discard : Array Card
    }


init : () -> ( Model, Cmd none )
init _ =
    ( { deck = fromList [ Zero, One, MinusOne, Two, MinusTwo, Crit, Null ], discard = Array.empty }, Cmd.none )



-- UPDATE


type Msg
    = Draw
    | HandleDrawResult Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model
            , Random.generate HandleDrawResult (Random.int 0 (Array.length model.deck - 1))
            )

        HandleDrawResult result ->
            let
                drawnCard : Maybe Card
                drawnCard =
                    Array.get result model.deck

                newDiscard : Array Card
                newDiscard =
                    case drawnCard of
                        Just card ->
                            Array.push card model.discard

                        Nothing ->
                            model.discard
            in
            ( { model | discard = newDiscard }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardRow : Card -> Html msg
cardRow card =
    case card of
        Zero ->
            li [] [ text "Zero" ]

        One ->
            li [] [ text "One" ]

        MinusOne ->
            li [] [ text "MinusOne" ]

        Two ->
            li [] [ text "Two" ]

        MinusTwo ->
            li [] [ text "MinusTwo" ]

        Crit ->
            li [] [ text "Crit" ]

        Null ->
            li [] [ text "Null" ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Draw ] [ text "Draw" ]
        , div [] [ text "Deck:" ]
        , ul [] (Array.toList (Array.map cardRow model.deck))
        , div [] [ text "Drawn cards:" ]
        , ul [] (Array.toList (Array.map cardRow model.discard))
        ]
