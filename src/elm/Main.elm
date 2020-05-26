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
    | Cis
    | D
    | Dis
    | E
    | F
    | Fis
    | G
    | Gis
    | A
    | Bb
    | B


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
    | Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Mandalorian
    | MelodicMinor
    | HarmonicMinor
    | MajorPentatonic
    | MinorPentatonic
    | Chromatic
    | Wholestep


type Range
    = OneOctave Int
    | OneStringScale Int
    | FullRange


type Message
    = Info String
    | Success String
    | Error String



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
    , showSettings : Bool
    , message : Maybe Message
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateEverything Scales )


allRoots : List Root
allRoots =
    [ C, Cis, D, Dis, E, F, Fis, G, Gis, A, Bb, B ]


rootToString : Root -> String
rootToString root =
    case root of
        C ->
            "C"

        Cis ->
            "C# / Db"

        D ->
            "D"

        Dis ->
            "D# / Eb"

        E ->
            "E"

        F ->
            "F"

        Fis ->
            "F# / Db"

        G ->
            "G"

        Gis ->
            "G# / Ab"

        A ->
            "A"

        Bb ->
            "Bb"

        B ->
            "B"


allScales : List Mode
allScales =
    [ Ionian
    , Dorian
    , Phrygian
    , Lydian
    , Mixolydian
    , Aeolian
    , Mandalorian
    , MelodicMinor
    , HarmonicMinor
    , MajorPentatonic
    , MinorPentatonic
    , Chromatic
    , Wholestep
    ]


allChords : List Mode
allChords =
    [ Dur, Moll, Dim, Augm, Sus2, Sus4 ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Dur ->
            "Dur"

        Moll ->
            "Moll"

        Dim ->
            "Dim"

        Augm ->
            "Augm"

        Sus2 ->
            "Sus2"

        Sus4 ->
            "Sus4"

        Ionian ->
            "Ionian - Major"

        Dorian ->
            "Dorian"

        Phrygian ->
            "Phrygian"

        Lydian ->
            "Lydian"

        Mixolydian ->
            "Mixolydian"

        Aeolian ->
            "Aeolian Natural Minor"

        Mandalorian ->
            "Mandalorian"

        MelodicMinor ->
            "Melodic Minor"

        HarmonicMinor ->
            "Harmonic Minor"

        MajorPentatonic ->
            "Major Pentatonic"

        MinorPentatonic ->
            "Minor Pentatonic"

        Chromatic ->
            "Chromatic"

        Wholestep ->
            "Wholestep"


allIntervals : List Int
allIntervals =
    [ 3, 4, 5, 6 ]


allRanges : List Range
allRanges =
    [ OneOctave 1
    , OneOctave 2
    , OneOctave 3
    , OneOctave 4
    , OneStringScale 1
    , OneStringScale 2
    , OneStringScale 3
    , OneStringScale 4
    , FullRange
    ]


rangeToString : Range -> String
rangeToString range =
    case range of
        OneStringScale n ->
            "One String Scale " ++ String.fromInt n

        OneOctave n ->
            "One Octave " ++ String.fromInt n

        FullRange ->
            "Full Range"


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
    , mode = Ionian
    , interval = 3
    , range = OneOctave 1
    , pattern = "♺ x 1"
    , elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = False
    , message = Just (Success "hello there")
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
    | ToggleSettings


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
              }
            , generateEverything model.topic
            )

        ToggleSettings ->
            ( { model
                | showSettings = not model.showSettings
              }
            , Cmd.none
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
        , infoBox model.message
        , selection model
        , settings model
        ]


selection model =
    div [ class "container flex-col mx-auto font-mono justify-center p-3 bg-gray-300 px-4" ]
        [ selectionItem model.practiceMode
            (\practice ->
                case practice of
                    TimeLimit ->
                        "Time limit"

                    ExerciseLimit ->
                        "Exercise limit"
            )
            "Practice mode: "
        , selectionItem model.topic
            (\topic ->
                case topic of
                    Scales ->
                        "Scales"

                    Chords ->
                        "Chords"

                    Doublestops ->
                        "Doublestops"
            )
            "Topic: "
        , div [ class "container text-left bg-gray mb-1 p-2" ]
            []
        , selectionItem model.root rootToString "Root: "
        , if model.topic == Doublestops then
            selectionItem model.interval String.fromInt "Interval: "

          else
            div [ class "hidden" ] []
        , selectionItem model.mode modeToString "Mode: "
        , selectionItem model.range rangeToString "Range: "
        , selectionItem model.pattern identity "Pattern: "
        , div [ class "container p-3 flex" ]
            [ button [ class primaryButton, class "flex-auto m-2", onClick NewExercise ] [ text "New exercise" ]
            , button [ class primaryButton, class "flex-auto m-2", onClick NextTopic ] [ text "Next topic" ]
            , button [ class secondaryButton, class "flex-auto m-2", onClick ToggleSettings ] [ text "..." ]
            ]
        ]


infoBox : Maybe Message -> Html msg
infoBox message =
    let
        ( color, content ) =
            case message of
                Just (Info msg) ->
                    ( "yellow-300", msg )

                Just (Error msg) ->
                    ( "red-300", msg )

                Just (Success msg) ->
                    ( "green-300", msg )

                Nothing ->
                    ( "", "" )
    in
    if String.isEmpty color then
        div [] []

    else
        div
            [ class <| "container flex-col mx-auto font-mono justify-center bg-" ++ color ++ " px-4" ]
            [ div [ class <| "container text-left bg-" ++ color ++ " mb-1 p-2" ]
                [ text content
                ]
            ]


selectionItem : a -> (a -> String) -> String -> Html msg
selectionItem item toString label =
    div [ class "container text-left bg-white mb-1 p-2" ]
        [ text label
        , text <| toString item
        ]


settings model =
    if model.showSettings then
        div [] [ text "yo" ]

    else
        div [] []


primaryButton =
    """bg-pink-500 hover:bg-pink-400 cursor-pointer text-white 
    font-bold py-2 px-4 border-b-4 border-pink-700 hover:border-pink-500 rounded"""


secondaryButton =
    """bg-gray-500 hover:bg-gray-400 cursor-pointer text-white 
    font-bold py-2 px-4 border-b-4 border-gray-700 hover:border-gray-500 rounded"""


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
