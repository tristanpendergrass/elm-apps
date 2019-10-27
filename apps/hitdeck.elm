module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Html, button, div, h1, hr, li, text, ul)
import Html.Events exposing (onClick)
import Maybe
import Random
import Random.List exposing (shuffle)
import Task
import Time


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
    , seed : Random.Seed
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
            , { id = 18, cardType = Zero }
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
            , { id = 38, cardType = Zero }
            ]
        }
    , discard = { id = 40, cards = [] }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mats = [ defaultMat, defaultMatTwo ]
      , nonce = 41
      , seed = Random.initialSeed 0
      }
    , Task.perform GenerateSeed Time.now
    )



-- UPDATE


type Msg
    = GenerateSeed Time.Posix
    | Reshuffle Mat
    | Draw Mat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateSeed time ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis time) }, Cmd.none )

        Reshuffle mat ->
            let
                allCards : List Card
                allCards =
                    List.concat [ mat.deck.cards, mat.discard.cards ]

                ( shuffledCards, newSeed ) =
                    Random.step (shuffle allCards) model.seed

                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = shuffledCards }

                oldDiscard : Pile
                oldDiscard =
                    mat.discard

                newDiscard : Pile
                newDiscard =
                    { oldDiscard | cards = [] }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck, discard = newDiscard }

                newMats : List Mat
                newMats =
                    List.map
                        (\m ->
                            if m == mat then
                                newMat

                            else
                                m
                        )
                        model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        Draw mat ->
            case mat.deck.cards of
                [] ->
                    ( model, Cmd.none )

                firstCard :: restOfCards ->
                    let
                        ( drawnCard, newSeed ) =
                            Random.step (Random.uniform firstCard restOfCards) model.seed

                        oldDiscard : Pile
                        oldDiscard =
                            mat.discard

                        newDiscard : Pile
                        newDiscard =
                            { oldDiscard | cards = drawnCard :: oldDiscard.cards }

                        oldDeck : Pile
                        oldDeck =
                            mat.deck

                        newDeck : Pile
                        newDeck =
                            { oldDeck | cards = List.filter ((/=) drawnCard) oldDeck.cards }

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
                    ( { model | mats = newMats, seed = newSeed }, Cmd.none )



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
        [ div [] [ button [ onClick (Draw mat) ] [ text "Draw" ], button [ onClick (Reshuffle mat) ] [ text "Reshuffle" ] ]
        , div [] [ text "Deck:" ]
        , ul [] (List.map cardRow mat.deck.cards)
        , div [] [ text "Drawn cards:" ]
        , ul [] (List.map cardRow mat.discard.cards)
        , hr [] []
        ]


view : Model -> Html Msg
view model =
    div []
        (List.map renderMat model.mats)
