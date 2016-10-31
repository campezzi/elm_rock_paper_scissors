module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


main =
    App.beginnerProgram { model = init, update = update, view = view }



-- MODEL


type alias Model =
    { myChoice : Maybe Choice
    , opponentChoice : Maybe Choice
    , outcome : Maybe Outcome
    }


type Choice
    = Rock
    | Paper
    | Scissors
    | Lizard
    | Spock


type Outcome
    = Draw
    | Victory
    | Defeat


init : Model
init =
    { myChoice = Nothing
    , opponentChoice = Nothing
    , outcome = Nothing
    }



-- UPDATE


type Msg
    = ChoicePicked Player Choice
    | Reset


type Player
    = Me
    | Opponent


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChoicePicked player choice ->
            case player of
                Me ->
                    updateOutcome { model | myChoice = Just choice }

                Opponent ->
                    updateOutcome { model | opponentChoice = Just choice }

        Reset ->
            init


updateOutcome : Model -> Model
updateOutcome model =
    { model | outcome = Maybe.map2 calculateOutcome model.myChoice model.opponentChoice }


defeatedBy : Choice -> List Choice
defeatedBy choice =
    case choice of
        Rock ->
            [ Scissors, Lizard ]

        Paper ->
            [ Rock, Spock ]

        Scissors ->
            [ Paper, Lizard ]

        Lizard ->
            [ Paper, Spock ]

        Spock ->
            [ Scissors, Rock ]


calculateOutcome : Choice -> Choice -> Outcome
calculateOutcome myChoice opponentChoice =
    if myChoice == opponentChoice then
        Draw
    else if (List.member opponentChoice (defeatedBy myChoice)) then
        Victory
    else
        Defeat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (outcomeString model.outcome) ]
        , div [] [ button [ onClick Reset ] [ text "RESET" ] ]
        , hr [] []
        , div [] [ h3 [] [ text "My Choice" ] ]
        , div [] (weaponButtons Me)
        , div [] [ h3 [] [ text "Opponent's Choice" ] ]
        , div [] (weaponButtons Opponent)
        , hr [] []
        , text (toString model)
        ]


weaponButtons : Player -> List (Html Msg)
weaponButtons player =
    let
        weaponButton =
            (\choice -> button [ onClick (ChoicePicked player choice) ] [ text (toString choice) ])
    in
        List.map weaponButton [ Rock, Paper, Scissors, Lizard, Spock ]


outcomeString : Maybe Outcome -> String
outcomeString outcome =
    case outcome of
        Just Draw ->
            "It's a draw!"

        Just Victory ->
            "You win!"

        Just Defeat ->
            "You lose!"

        _ ->
            "Game in progress"
