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


type
    Mode
    -- chords
    = Dur
    | Moll
    | Dim
    | Augm
    | Sus2
    | Sus4
      -- scales
    | Ionisch
    | Dorisch
    | Phrygisch
    | Lydisch
    | Mixolydisch
    | Aeolisch


type Range
    = OneOctave
    | TwoOctaves



-- type Pattern
--     = Slur Int
--     | Repeat Int
--     | Staccato Int
-- Model


type alias Model =
    { practiceMode : PracticeMode
    , topic : Topic
    , root : Root
    , mode : Mode
    , interval : Int
    , range : Range
    , pattern : String
    , elapsedTime : Int
    , completedExercises : Int
    , isRunning : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateEverything Scales )


allRoots : List Root
allRoots =
    [ C, G, D, A, F, Bb, Eb ]


allScales : List Mode
allScales =
    [ Ionisch, Dorisch, Phrygisch, Lydisch, Mixolydisch, Aeolisch ]


allChords : List Mode
allChords =
    [ Dur, Moll, Dim, Augm, Sus2, Sus4 ]


allIntervals : List Int
allIntervals =
    [ 3, 4, 5, 6 ]


allRanges : List Range
allRanges =
    [ OneOctave, TwoOctaves ]


allPatterns : List String
allPatterns =
    [ "▾ x 1"
    , "▾ x 2"
    , "▾ x 3"
    , "▾ x 4"
    , "▾ x 5"
    , "▾ x 6"
    , "▾ x 7"
    , "▾ x 8"
    , "▾ x 9"
    , "▾ x 10"
    , "▾ x 11"
    , "▾ x 12"
    , "⏜ x 1"
    , "⏜ x 2"
    , "⏜ x 3"
    , "⏜ x 4"
    , "⏜ x 5"
    , "⏜ x 6"
    , "⏜ x 7"
    , "⏜ x 8"
    , "⏜ x 9"
    , "⏜ x 10"
    , "⏜ x 11"
    , "⏜ x 12"
    , "♺ x 1"
    , "♺ x 2"
    , "♺ x 3"
    , "♺ x 4"
    , "♺ x 5"
    , "♺ x 6"
    , "♺ x 7"
    , "♺ x 8"
    , "♺ x 9"
    , "♺ x 10"
    , "♺ x 11"
    , "♺ x 12"
    ]


initialModel : Model
initialModel =
    { practiceMode = TimeLimit
    , topic = Scales
    , root = C
    , mode = Ionisch
    , interval = 3
    , range = OneOctave
    , pattern = "♺ x 1"
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
    | NewIntervalGenerated ( Maybe Int, List Int )
    | NewRangeGenerated ( Maybe Range, List Range )
    | NewPatternGenerated ( Maybe String, List String )
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
            , generateEverything model.topic
            )

        NextTopic ->
            let
                nextTopic =
                    case model.topic of
                        Scales ->
                            Chords

                        Chords ->
                            Doublestops

                        Doublestops ->
                            Scales
            in
            ( { model
                | completedExercises = model.completedExercises + 1
                , isRunning = False
                , topic = nextTopic
              }
            , generateEverything nextTopic
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

        NewIntervalGenerated ( maybeInterval, _ ) ->
            case maybeInterval of
                Just interval ->
                    ( { model | interval = interval }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewRangeGenerated ( maybeRange, _ ) ->
            case maybeRange of
                Just range ->
                    ( { model | range = range }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewPatternGenerated ( maybePattern, _ ) ->
            case maybePattern of
                Just pattern ->
                    ( { model | pattern = pattern }, Cmd.none )

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


generateEverything topic =
    Cmd.batch
        [ generateInterval
        , generateMode topic
        , generatePattern
        , generateRange
        , generateRoot
        ]


generateRoot : Cmd Msg
generateRoot =
    Random.generate NewRootGenerated (Random.List.choose allRoots)


generateRange : Cmd Msg
generateRange =
    Random.generate NewRangeGenerated (Random.List.choose allRanges)


generateMode : Topic -> Cmd Msg
generateMode topic =
    let
        source =
            case topic of
                Scales ->
                    allScales

                Chords ->
                    allChords

                Doublestops ->
                    allScales
    in
    Random.generate NewModeGenerated (Random.List.choose source)


generateInterval : Cmd Msg
generateInterval =
    Random.generate NewIntervalGenerated (Random.List.choose allIntervals)


generatePattern : Cmd Msg
generatePattern =
    Random.generate NewPatternGenerated (Random.List.choose allPatterns)



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
        , div
            [ class <|
                if model.topic == Doublestops then
                    "container text-left bg-white mb-1 p-2"

                else
                    "hidden"
            ]
            [ text "Interval:  "
            , text <| String.fromInt model.interval
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Mode:  "
            , text <| Debug.toString model.mode
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Range:  "
            , text <| Debug.toString model.range
            ]
        , div [ class "container text-left bg-white mb-1 p-2" ]
            [ text "Pattern:  "
            , text model.pattern
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
        -- [ h1 [ class "text-4xl font-sans" ] [ text "✔︎❒✘❍🎵" ]
        [ h1 [ class "text-4xl font-sans" ] [ text "❒" ]
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
