module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, br, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Random.List
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.batch [ generateMode, generateRoot ] )


allRoots : List Root
allRoots =
    [ C, G, D, A, F, Bb, Eb ]


allModes : List Mode
allModes =
    [ Dur, Moll ]


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
    | NewRootGenerated ( Maybe Root, List Root )
    | NewModeGenerated ( Maybe Mode, List Mode )
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
            , Cmd.batch [ generateRoot, generateMode ]
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

        NewModeGenerated ( maybeMode, _ ) ->
            case maybeMode of
                Just mode ->
                    ( { model | mode = mode }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewRootGenerated ( maybeRoot, _ ) ->
            case maybeRoot of
                Just root ->
                    ( { model | root = root }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


clearTimer : Model -> ( Model, Cmd Msg )
clearTimer model =
    ( { model | isRunning = False, elapsedTime = 0 }, Cmd.none )


toggleTimer : Model -> ( Model, Cmd Msg )
toggleTimer model =
    if model.isRunning then
        ( { model | isRunning = False }, Cmd.none )

    else
        ( { model | isRunning = True }, Cmd.none )


generateRoot : Cmd Msg
generateRoot =
    Random.generate NewRootGenerated (Random.List.choose allRoots)


generateMode : Cmd Msg
generateMode =
    Random.generate NewModeGenerated (Random.List.choose allModes)



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
    div [ class "container mx-auto bg-gray-200 px-5 py-5 my-10 max-w-lg" ]
        [ header model
        , selection model
        , settings model
        ]


selection model =
    div [ class "container flex-col mx-auto font-mono justify-center p-3 bg-gray-300 px-4" ]
        [ div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Practice mode: "
            , text <| Debug.toString model.practiceMode
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Topic: "
            , text <| Debug.toString model.topic
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Root:  "
            , text <| Debug.toString model.root
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Mode:  "
            , text <| Debug.toString model.mode
            ]
        , div [ class "container p-3 flex" ]
            [ button [ class primaryButton, class "flex-auto m-2", onClick NewExercise ] [ text "New exercise" ]
            , button [ class primaryButton, class "flex-auto m-2", onClick NextTopic ] [ text "Next topic" ]
            ]
        ]


settings model =
    div [] []


primaryButton =
    "bg-pink-500 hover:bg-pink-400 cursor-pointer text-white font-bold py-2 px-4 border-b-4 border-pink-700 hover:border-pink-500 rounded"


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

        elementClass =
            "m-2 px-2 bg-gray-100"
    in
    div [ class "container inline-flex flex flex-row font-mono" ]
        [ h1 [ class "font-mono text-5xl uppercase" ] [ text "Rüben" ]
        , div [ class "container flex justify-end items-start" ]
            [ div [ class elementClass ]
                [ text (toDoubleDigits minutes ++ ":" ++ toDoubleDigits seconds)
                ]
            , button [ class elementClass, onClick ToggleTimer ]
                [ text <|
                    if model.isRunning then
                        "pause"

                    else
                        "start"
                ]
            , button [ class elementClass, onClick ClearTimer ] [ text "■" ]
            , div [ class elementClass ]
                [ text ("ex. " ++ String.fromInt model.completedExercises)
                ]
            ]
        ]
