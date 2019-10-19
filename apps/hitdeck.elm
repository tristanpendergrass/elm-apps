module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Html, button, div, h1, li, text, ul)
import Html.Events exposing (onClick)
import Random


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type CardType
    = Zero
    | One
    | MinusOne
    | Two
    | MinusTwo
    | Crit
    | Null


type alias CardId =
    Int


type alias Card =
    { id : CardId, cardType : CardType }


type alias Model =
    { deck : Array Card
    , discard : Array Card
    , nonce : Int -- Number to be used for card ids
    }


init : () -> ( Model, Cmd none )
init _ =
    ( { deck =
            fromList
                [ { id = 0, cardType = Zero }
                , { id = 1, cardType = One }
                , { id = 2, cardType = MinusOne }
                , { id = 3, cardType = Two }
                , { id = 4, cardType = MinusTwo }
                , { id = 5, cardType = Crit }
                , { id = 6, cardType = Null }
                ]
      , discard = Array.empty
      , nonce = 7
      }
    , Cmd.none
    )



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

                isDrawnCard : Card -> Card -> Bool
                isDrawnCard left right =
                    left == right

                newDeck : Array Card
                newDeck =
                    case drawnCard of
                        Just card ->
                            Array.filter ((/=) card) model.deck

                        Nothing ->
                            model.deck
            in
            ( { model | discard = newDiscard, deck = newDeck }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardRow : Card -> Html Msg
cardRow card =
    case card.cardType of
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
