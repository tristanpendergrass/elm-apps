module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Html, button, div, h1, li, text, ul)
import Html.Events exposing (onClick)
import Random


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Id =
    Int


type CardType
    = Zero
    | One
    | MinusOne
    | Two
    | MinusTwo
    | Crit
    | Null


type alias CardData =
    { cardType : CardType }


type alias Card =
    ( Id, CardData )


type alias PileData =
    { cards : List Card }


type alias Pile =
    ( Id, PileData )


type alias MatData =
    { deck : Pile, discard : Pile }


type alias Mat =
    ( Id, MatData )


type alias Model =
    { mats : List Mat
    , nonce : Int -- Number to be used and then incremented when assigning new ids
    }


defaultDeck : PileData
defaultDeck =
    { cards = [] }


defaultDiscard : PileData
defaultDiscard =
    { cards = [] }


defaultMat : MatData
defaultMat =
    { deck = ( 1, defaultDeck )
    , discard = ( 2, defaultDiscard )
    }


init : () -> ( Model, Cmd none )
init _ =
    ( { mats = [ ( 0, defaultMat ) ]
      , nonce = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Draw Mat
    | HandleDrawResult Mat Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw mat ->
            let
                deckData : PileData
                deckData =
                    Tuple.second (Tuple.second mat).deck
            in
            ( model
            , Random.generate (HandleDrawResult mat) (Random.int 0 (List.length deckData.cards - 1))
            )

        HandleDrawResult mat result ->
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
