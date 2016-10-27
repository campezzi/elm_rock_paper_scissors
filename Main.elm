module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


main =
    App.beginnerProgram { model = init, update = update, view = view }



-- MODEL


type alias Model =
    { myChoice : Maybe Choice
    , theirChoice : Maybe Choice
    , outcome : Maybe Outcome
    }


type Choice
    = Rock
    | Paper
    | Scissors


type Outcome
    = Draw
    | Victory
    | Defeat


init : Model
init =
    { myChoice = Nothing
    , theirChoice = Nothing
    , outcome = Nothing
    }



-- UPDATE


type Msg
    = PlayerAPickChoice Choice
    | PlayerBPickChoice Choice
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlayerAPickChoice choice ->
            let
                updatedModel =
                    { model | myChoice = Just choice }
            in
                updateOutcome updatedModel

        PlayerBPickChoice choice ->
            let
                updatedModel =
                    { model | theirChoice = Just choice }
            in
                updateOutcome updatedModel

        Reset ->
            init


updateOutcome : Model -> Model
updateOutcome model =
    case model.myChoice of
        Just myChoice ->
            case model.theirChoice of
                Just theirChoice ->
                    { model | outcome = Just (calculateOutcome myChoice theirChoice) }

                Nothing ->
                    model

        Nothing ->
            model


defeatedBy : Choice -> List Choice
defeatedBy choice =
    case choice of
        Rock ->
            [ Scissors ]

        Paper ->
            [ Rock ]

        Scissors ->
            [ Paper ]


calculateOutcome : Choice -> Choice -> Outcome
calculateOutcome choiceA choiceB =
    if choiceA == choiceB then
        Draw
    else if (List.member choiceB (defeatedBy choiceA)) then
        Victory
    else
        Defeat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick (PlayerAPickChoice Rock) ] [ text "Player A Rock" ]
            , button [ onClick (PlayerAPickChoice Paper) ] [ text "Player A Paper" ]
            , button [ onClick (PlayerAPickChoice Scissors) ] [ text "Player A Scissors" ]
            , button [ onClick (PlayerBPickChoice Rock) ] [ text "Player B Rock" ]
            , button [ onClick (PlayerBPickChoice Paper) ] [ text "Player B Paper" ]
            , button [ onClick (PlayerBPickChoice Scissors) ] [ text "Player B Scissors" ]
            , button [ onClick Reset ] [ text "RESET" ]
            ]
        , text (toString model)
        ]
