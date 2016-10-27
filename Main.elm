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
    = MyChoice Choice
    | TheirChoice Choice
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        MyChoice choice ->
            let
                updatedModel =
                    { model | myChoice = Just choice }
            in
                updateOutcome updatedModel

        TheirChoice choice ->
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
calculateOutcome myChoice theirChoice =
    if myChoice == theirChoice then
        Draw
    else if (List.member theirChoice (defeatedBy myChoice)) then
        Victory
    else
        Defeat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick (MyChoice Rock) ] [ text "ME - Rock" ]
            , button [ onClick (MyChoice Paper) ] [ text "ME - Paper" ]
            , button [ onClick (MyChoice Scissors) ] [ text "ME - Scissors" ]
            ]
        , div []
            [ button [ onClick (TheirChoice Rock) ] [ text "THEM - Rock" ]
            , button [ onClick (TheirChoice Paper) ] [ text "THEM - Paper" ]
            , button [ onClick (TheirChoice Scissors) ] [ text "THEM - Scissors" ]
            ]
        , div [] [ button [ onClick Reset ] [ text "RESET" ] ]
        , text (toString model)
        ]
