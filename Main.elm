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
    , Random.generate OpponentChoice (Random.int 0 4)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = ChoicePicked Player Choice
    | OpponentChoice Int
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

        OpponentChoice randomInt ->
            ( updateOutcome { model | opponentChoice = Just (choiceForIndex randomInt) }, Cmd.none )

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
        , h2 [] [ text (outcomeString model.outcome) ]
        , div [] [ button [ onClick Reset ] [ text "RESET" ] ]
        ]


choiceButtons : Player -> List (Html Msg)
choiceButtons player =
    let
        choiceButton =
            (\choice -> button [ onClick (ChoicePicked player choice) ] [ text (toString choice) ])
    in
        List.map choiceButton allChoices


allChoices : List Choice
allChoices =
    [ Rock, Paper, Scissors, Lizard, Spock ]


choiceForIndex : Int -> Choice
choiceForIndex index =
    Maybe.withDefault Rock (List.drop index allChoices |> List.head)


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
