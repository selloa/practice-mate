module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, br, button, div, h1, p, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type PracticeMode
    = TimeLimit
    | ExerciseLimit


type Topic
    = Scales
    | Chords
    | Doublestops


type Root
    = C
    | G
    | D
    | A
    | F
    | Bb
    | Eb


type Mode
    = Dur
    | Moll


type Range
    = OneOctave
    | TwoOctaves


type Pattern
    = Slur Int
    | Repeat Int
    | Staccato Int



-- Model


type alias Model =
    { practiceMode : PracticeMode
    , topic : Topic
    , root : Root
    , mode : Mode
    , range : Maybe Range
    , pattern : Maybe Pattern
    , elapsedTime : Int
    , completedExercises : Int
    , isRunning : Bool
    }


initialModel : Model
initialModel =
    { practiceMode = TimeLimit
    , topic = Scales
    , root = C
    , mode = Dur
    , range = Nothing
    , pattern = Nothing
    , elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    }



-- update


type Msg
    = Tick Time.Posix
    | ToggleTimer
    | ClearTimer
    | KeyPressed String
    | NewExercise
    | NextTopic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.isRunning then
                ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleTimer ->
            toggleTimer model

        ClearTimer ->
            clearTimer model

        KeyPressed key ->
            if key == " " then
                toggleTimer model

            else if key == "Backspace" then
                clearTimer model

            else
                ( model, Cmd.none )

        NewExercise ->
            ( { model
                | completedExercises = model.completedExercises + 1
                , isRunning = False
              }
            , Cmd.none
            )

        NextTopic ->
            ( { model
                | completedExercises = model.completedExercises + 1
                , isRunning = False
                , topic =
                    case model.topic of
                        Scales ->
                            Chords

                        Chords ->
                            Doublestops

                        Doublestops ->
                            Scales
              }
            , Cmd.none
            )


clearTimer model =
    ( { model | isRunning = False, elapsedTime = 0 }, Cmd.none )


toggleTimer model =
    if model.isRunning then
        ( { model | isRunning = False }, Cmd.none )

    else
        ( { model | isRunning = True }, Cmd.none )



-- subscriptions


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDecoder
        , Time.every 1000 Tick
        ]



-- view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Rüben" ]
        , header model
        , selection model
        , settings model
        ]


selection model =
    div []
        [ text "Practice mode: "
        , text <| Debug.toString model.practiceMode
        , br [] []
        , text "Topic: "
        , text <| Debug.toString model.topic
        , br [] []
        , text "Root:  "
        , text <| Debug.toString model.root
        , br [] []
        , text "Mode:  "
        , text <| Debug.toString model.mode
        , br [] []
        , button [ onClick NewExercise ] [ text "New exercise" ]
        , br [] []
        , button [ onClick NextTopic ] [ text "Next topic" ]
        ]


settings model =
    div [] []


header model =
    let
        minutes =
            String.fromInt (model.elapsedTime // 60)

        seconds =
            String.fromInt (remainderBy 60 model.elapsedTime)

        toDoubleDigits number =
            if String.length number < 2 then
                "0" ++ number

            else
                number
    in
    div []
        [ text (toDoubleDigits minutes ++ ":" ++ toDoubleDigits seconds)
        , button [ onClick ToggleTimer ]
            [ text <|
                if model.isRunning then
                    "▐▐ pause"

                else
                    "▶ ️ play"
            ]
        , button [ onClick ClearTimer ] [ text "■" ]
        , br [] []
        , text <| "finished exercises: " ++ String.fromInt model.completedExercises
        ]
