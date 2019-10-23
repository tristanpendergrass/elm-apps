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


type alias Card =
    { id : Id, cardType : CardType }


type alias Pile =
    { id : Id, cards : List Card }


type alias Mat =
    { id : Id, deck : Pile, discard : Pile }


type alias Model =
    { mats : List Mat
    , nonce : Int -- Number to be used and then incremented when assigning new ids
    }


defaultDeck : Pile
defaultDeck =
    { id = 0, cards = [] }


defaultDiscard : Pile
defaultDiscard =
    { id = 1, cards = [] }


defaultMat : Mat
defaultMat =
    { id = 2
    , deck = defaultDeck
    , discard = defaultDiscard
    }


init : () -> ( Model, Cmd none )
init _ =
    ( { mats = [ defaultMat ]
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
            ( model
            , Random.generate (HandleDrawResult mat) (Random.int 0 (List.length mat.deck.cards - 1))
            )

        HandleDrawResult mat result ->
            let
                cards : List Card
                cards =
                    mat.deck.cards

                drawnCard : Maybe Card
                drawnCard =
                    Array.get result (Array.fromList cards)

                newDiscard : Pile
                newDiscard =
                    let
                        discard : Pile
                        discard =
                            mat.discard
                    in
                    case drawnCard of
                        Just card ->
                            { discard | cards = card :: discard.cards }

                        Nothing ->
                            mat.discard

                newDeck : Pile
                newDeck =
                    let
                        deck : Pile
                        deck =
                            mat.deck
                    in
                    case drawnCard of
                        Just card ->
                            { deck | cards = List.filter ((/=) card) cards }

                        Nothing ->
                            mat.deck
            in
            ( { model | mats = [ Mat 2 newDeck newDiscard ] }, Cmd.none )



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
    let
        ( deck, discard ) =
            case List.head model.mats of
                Just mat ->
                    ( mat.deck, mat.discard )

                Nothing ->
                    ( Pile 5 [], Pile 6 [] )
    in
    div []
        [ button [ onClick Draw ] [ text "Draw" ]
        , div [] [ text "Deck:" ]
        , ul [] (List.map cardRow deck)
        , div [] [ text "Drawn cards:" ]
        , ul [] (Array.toList (Array.map cardRow model.discard))
        ]
