module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, fromList)
import Browser
import Html exposing (Html, button, div, h1, hr, li, text, ul)
import Html.Events exposing (onClick)
import List.Extra
import Maybe
import Random
import Random.List exposing (shuffle)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


replace : a -> a -> List a -> List a
replace oldA newA =
    List.map
        (\ax ->
            if ax == oldA then
                newA

            else
                ax
        )



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


type EditState
    = Default
    | Editing


type alias Mat =
    { id : Id, deck : Pile, discard : Pile, editState : EditState }


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
    , editState = Default
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mats = [ defaultMat ]
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
    | AddCard Mat CardType
    | RemoveCard Pile Mat Card
    | ToggleMatEdit Mat
    | AddMat


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
                    replace mat newMat model.mats
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
                            replace mat newMat model.mats
                    in
                    ( { model | mats = newMats, seed = newSeed }, Cmd.none )

        AddCard mat cardType ->
            let
                oldDeck : Pile
                oldDeck =
                    mat.deck

                newDeck : Pile
                newDeck =
                    { oldDeck | cards = Card model.nonce cardType :: oldDeck.cards }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats, nonce = model.nonce + 1 }, Cmd.none )

        RemoveCard deck mat card ->
            let
                newDeck : Pile
                newDeck =
                    { deck | cards = List.Extra.remove card deck.cards }

                newMat : Mat
                newMat =
                    { mat | deck = newDeck }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        ToggleMatEdit mat ->
            let
                newMat : Mat
                newMat =
                    { mat
                        | editState =
                            if mat.editState == Default then
                                Editing

                            else
                                Default
                    }

                newMats : List Mat
                newMats =
                    replace mat newMat model.mats
            in
            ( { model | mats = newMats }, Cmd.none )

        AddMat ->
            let
                newMat : Mat
                newMat =
                    { id = model.nonce
                    , deck = { id = model.nonce + 1, cards = [] }
                    , discard = { id = model.nonce + 2, cards = [] }
                    , editState = Default
                    }
            in
            ( { model | mats = newMat :: model.mats, nonce = model.nonce + 3 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


cardRow : Mat -> Card -> Html Msg
cardRow mat card =
    case card.cardType of
        Zero ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "Zero" ]

        One ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "One" ]

        MinusOne ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "MinusOne" ]

        Two ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "Two" ]

        MinusTwo ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "MinusTwo" ]

        Crit ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "Crit" ]

        Null ->
            li [] [ button [ onClick (RemoveCard mat.deck mat card) ] [ text "-" ], text "Null" ]


renderAddCard : Mat -> CardType -> Html Msg
renderAddCard mat cardType =
    let
        label : String
        label =
            case cardType of
                Zero ->
                    "+Zero"

                One ->
                    "+One"

                MinusOne ->
                    "+MinusOne"

                Two ->
                    "+Two"

                MinusTwo ->
                    "+MinusTwo"

                Crit ->
                    "+Crit"

                Null ->
                    "+Null"
    in
    button [ onClick (AddCard mat cardType) ] [ text label ]


renderMat : Mat -> Html Msg
renderMat mat =
    div []
        [ div []
            [ button [ onClick (ToggleMatEdit mat) ] [ text "Toggle Editing" ] ]
        , div []
            [ renderAddCard mat Zero
            , renderAddCard mat One
            , renderAddCard mat MinusOne
            , renderAddCard mat Two
            , renderAddCard mat MinusTwo
            , renderAddCard mat Crit
            , renderAddCard mat Null
            ]
        , div [] [ button [ onClick (Draw mat) ] [ text "Draw" ], button [ onClick (Reshuffle mat) ] [ text "Reshuffle" ] ]
        , div [] [ text "Deck:" ]
        , ul [] (List.map (cardRow mat) mat.deck.cards)
        , div [] [ text "Drawn cards:" ]
        , ul [] (List.map (cardRow mat) mat.discard.cards)
        , hr [] []
        ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddMat ] [ text "Add Mat" ]
        , hr [] []
        , div []
            (List.map renderMat model.mats)
        ]
