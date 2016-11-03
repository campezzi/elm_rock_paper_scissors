module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Random


main =
    App.program { init = init, update = update, view = view, subscriptions = subscriptions }



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


init : ( Model, Cmd Msg )
init =
    ( { myChoice = Nothing
      , opponentChoice = Nothing
      , outcome = Nothing
      }
    , Random.generate (ChoicePicked Opponent) generateChoice
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


allChoices : List Choice
allChoices =
    [ Rock, Paper, Scissors, Lizard, Spock ]



-- UPDATE


type Msg
    = ChoicePicked Player Choice
    | Reset


type Player
    = Me
    | Opponent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChoicePicked player choice ->
            case player of
                Me ->
                    ( updateOutcome { model | myChoice = Just choice }, Cmd.none )

                Opponent ->
                    ( updateOutcome { model | opponentChoice = Just choice }, Cmd.none )

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
        [ div [] [ h3 [] [ text "My Choice" ] ]
        , div [] (choiceButtons Me)
        , hr [] []
        , h2 [] [ text (outcomeString model) ]
        , div [] [ button [ onClick Reset ] [ text "RESET" ] ]
        ]


choiceButtons : Player -> List (Html Msg)
choiceButtons player =
    let
        choiceButton =
            (\choice -> button [ onClick (ChoicePicked player choice) ] [ text (toString choice) ])
    in
        List.map choiceButton allChoices


generateChoice : Random.Generator Choice
generateChoice =
    let
        choiceForIndex =
            (\index -> Maybe.withDefault Rock (List.drop index allChoices |> List.head))
    in
        Random.map choiceForIndex (Random.int 0 ((List.length allChoices) - 1))


outcomeString : Model -> String
outcomeString model =
    case model.outcome of
        Just Draw ->
            "It's a draw! You both picked " ++ (opponentChoiceString model)

        Just Victory ->
            "You win! Your opponent picked " ++ (opponentChoiceString model)

        Just Defeat ->
            "You lose! Your opponent picked " ++ (opponentChoiceString model)

        _ ->
            "Game in progress"


opponentChoiceString : Model -> String
opponentChoiceString model =
    case model.opponentChoice of
        Just choice ->
            toString choice

        Nothing ->
            ""
