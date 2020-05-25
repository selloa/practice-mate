module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
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
    , range : Maybe Range
    , pattern : Maybe Pattern
    , elapsedTime : Int
    , completedExercises : Int
    , isRunning : Bool
    , key : String
    }


initialModel : Model
initialModel =
    { practiceMode = TimeLimit
    , topic = Scales
    , root = C
    , range = Nothing
    , pattern = Nothing
    , elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , key = ""
    }



-- update


type Msg
    = Tick Time.Posix
    | ToggleTimer
    | ClearTimer
    | KeyPressed String
    | NoOp


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
            ( { model | isRunning = False, elapsedTime = 0 }, Cmd.none )

        KeyPressed key ->
            if key == " " then
                toggleTimer model

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
        [ onKeyPress keyDecoder
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
        , text model.key
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
        , br [] []
        , text <| "finished exercises: " ++ String.fromInt model.completedExercises
        , br [] []
        , button [ onClick ToggleTimer ]
            [ text <|
                if model.isRunning then
                    "▐▐ pause"

                else
                    "▶ ️ play"
            ]
        , button [ onClick ClearTimer ] [ text "■" ]
        ]
