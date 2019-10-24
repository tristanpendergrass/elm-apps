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


defaultMat : Mat
defaultMat =
    { id = 2
    , deck =
        { id = 10
        , cards =
            [ { id = 11, cardType = Zero }
            , { id = 12, cardType = One }
            , { id = 13, cardType = MinusOne }
            , { id = 14, cardType = Two }
            , { id = 15, cardType = MinusTwo }
            , { id = 16, cardType = Crit }
            , { id = 17, cardType = Null }
            ]
        }
    , discard = { id = 20, cards = [] }
    }


defaultMatTwo : Mat
defaultMatTwo =
    { id = 3
    , deck =
        { id = 30
        , cards =
            [ { id = 31, cardType = Zero }
            , { id = 32, cardType = One }
            , { id = 33, cardType = MinusOne }
            , { id = 34, cardType = Two }
            , { id = 35, cardType = MinusTwo }
            , { id = 36, cardType = Crit }
            , { id = 37, cardType = Null }
            ]
        }
    , discard = { id = 40, cards = [] }
    }


init : () -> ( Model, Cmd none )
init _ =
    ( { mats = [ defaultMat, defaultMatTwo ]
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

                newMat : Mat
                newMat =
                    { mat | deck = newDeck, discard = newDiscard }

                newMats : List Mat
                newMats =
                    List.map
                        (\l ->
                            if l == mat then
                                newMat

                            else
                                l
                        )
                        model.mats
            in
            ( { model | mats = newMats }, Cmd.none )



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


renderMat : Mat -> Html Msg
renderMat mat =
    div []
        [ button [ onClick (Draw mat) ] [ text "Draw" ]
        , div [] [ text "Deck:" ]
        , ul [] (List.map cardRow mat.deck.cards)
        , div [] [ text "Drawn cards:" ]
        , ul [] (List.map cardRow mat.discard.cards)
        ]


view : Model -> Html Msg
view model =
    div []
        (List.map renderMat model.mats)
