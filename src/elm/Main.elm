port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Attribute, Html, button, div, h1, input, text)
import Html.Attributes as A exposing (class, max, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Random.List
import Time



-- ports


port printToConsole : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel

        configuration =
            createConfiguration newModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encodeConfiguration configuration), cmds ]
    )



-- main


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


type PracticeMode
    = TimeLimit Int
    | ExerciseLimit Int


type Topic
    = Scales
    | Chords
    | Doublestops


type Root
    = A
    | Bb
    | B
    | C
    | Cis
    | D
    | Dis
    | E
    | F
    | Fis
    | G
    | Gis


type Key
    = Ionian
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
    | Blues


type Chord
    = Major
    | Minor
    | Dim
    | Aug
    | Sus2
    | Sus4
    | Maj7
    | Min7
    | Dom7
    | MinMaj7
    | HalfDim7
    | Dim7


type Range
    = NoEmptyStrings
    | AllEmptyStrings
    | AString
    | DString
    | GString
    | CString
    | NoAString
    | NoADString


type Interval
    = Sixths
    | Thirds
    | Octaves
    | Fourths
    | Fifths
    | Tenths


type Message
    = Info String Int
    | Success String Int
    | Error String Int


type Bowing
    = Slured Int
    | RepeatedStaccato Int
    | RepeatedTenuto Int
    | BowStaccato Int
    | Sequenced
    | AddTopNote
    | Rhythmed


type Preset
    = Easy
    | All
    | None
    | Custom


type alias Configuration =
    { topics : List Topic
    , roots : List Root
    , keys : List Key
    , intervals : List Interval
    , ranges : List Range
    , bowings : List Bowing
    , chords : List Chord
    }


createConfiguration : Model -> Configuration
createConfiguration model =
    { topics = model.topics
    , roots = model.roots
    , keys = model.keys
    , intervals = model.intervals
    , ranges = model.ranges
    , bowings = model.bowings
    , chords = model.chords
    }



-- Model


type alias Model =
    { elapsedTime : Int
    , completedExercises : Int
    , isRunning : Bool
    , showSettings : Bool
    , message : Maybe Message
    , preset : Preset

    -- selection
    , practiceMode : PracticeMode
    , topic : Topic

    --
    , topics : List Topic
    , roots : List Root
    , keys : List Key
    , intervals : List Interval
    , ranges : List Range
    , bowings : List Bowing
    , chords : List Chord
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        localInitialModel =
            initialModel flags
    in
    ( localInitialModel
    , Cmd.none
      -- , shuffleEverything localInitialModel
    )


allRoots : List Root
allRoots =
    [ A, B, Bb, C, Cis, D, Dis, E, F, Fis, G, Gis ]


allTopics : List Topic
allTopics =
    [ Scales, Chords, Doublestops ]


rootToString : Root -> String
rootToString root =
    case root of
        A ->
            "A"

        Bb ->
            "Bb"

        B ->
            "B"

        C ->
            "C"

        Cis ->
            "C# / Db"

        D ->
            "D"

        Dis ->
            "Eb / D#"

        E ->
            "E"

        F ->
            "F"

        Fis ->
            "F# / Gb"

        G ->
            "G"

        Gis ->
            "G# / Ab"


allScales : List Key
allScales =
    [ Ionian
    , Aeolian
    , MelodicMinor
    , HarmonicMinor
    , Dorian
    , Phrygian
    , Lydian
    , Mixolydian
    , Mandalorian
    , MajorPentatonic
    , MinorPentatonic
    , Chromatic
    , Wholestep
    , Blues
    ]


allChords : List Chord
allChords =
    [ Major
    , Minor
    , Dim
    , Aug
    , Sus2
    , Sus4
    , Maj7
    , Min7
    , Dom7
    , MinMaj7
    , HalfDim7
    , Dim7
    ]


practiceModeToString : PracticeMode -> String
practiceModeToString mode =
    case mode of
        TimeLimit duration ->
            "⏲️ " ++ String.fromInt duration

        ExerciseLimit exercises ->
            "✔ (" ++ String.fromInt exercises ++ ")"


practiceModeToStringWithoutNumber : PracticeMode -> String
practiceModeToStringWithoutNumber mode =
    case mode of
        TimeLimit _ ->
            "Time limit"

        ExerciseLimit _ ->
            "Exercise limit"


topicToString : Topic -> String
topicToString topic =
    case topic of
        Scales ->
            "Scales"

        Chords ->
            "Chords"

        Doublestops ->
            "Doublestops"


chordToString : Chord -> String
chordToString chord =
    case chord of
        Major ->
            "Major"

        Minor ->
            "Minor"

        Dim ->
            "Dim"

        Aug ->
            "Augm"

        Sus2 ->
            "Sus2"

        Sus4 ->
            "Sus4"

        Maj7 ->
            "Maj7"

        Min7 ->
            "Min7"

        Dom7 ->
            "Dom7"

        MinMaj7 ->
            "MinMaj7"

        HalfDim7 ->
            "HalfDim7"

        Dim7 ->
            "Dim7"


keyToString : Key -> String
keyToString key =
    case key of
        Ionian ->
            "Major (Ionian)"

        Dorian ->
            "Dorian"

        Phrygian ->
            "Phrygian"

        Lydian ->
            "Lydian"

        Mixolydian ->
            "Mixolydian"

        Aeolian ->
            "Minor (Natural, Aeolian)"

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

        Blues ->
            "Blues"


bowingToString : Bowing -> String
bowingToString bowing =
    case bowing of
        Slured n ->
            "Slured " ++ String.fromInt n

        RepeatedStaccato n ->
            "Repeated Staccato " ++ String.fromInt n

        RepeatedTenuto n ->
            "Repeated Tenuto " ++ String.fromInt n

        BowStaccato n ->
            "Bow Staccato " ++ String.fromInt n

        Sequenced ->
            "Sequenced"

        AddTopNote ->
            "AddTopNote"

        Rhythmed ->
            "Rhythmed"


intervalToString : Interval -> String
intervalToString interval =
    case interval of
        Sixths ->
            "6ths"

        Thirds ->
            "3rds"

        Octaves ->
            "8ths"

        Fourths ->
            "parallel 4ths"

        Fifths ->
            "parallel 5ths"

        Tenths ->
            "10ths"


scalePatternToString : Key -> String
scalePatternToString key =
    case key of
        Ionian ->
            "X^X 2 2 3"

        Dorian ->
            "3 X^X^X 2"

        Phrygian ->
            "2 3 3 X^X"

        Lydian ->
            "X 2 2 3 3"

        Mixolydian ->
            "X^X^X 2 2"

        Aeolian ->
            "3 3 X^X^X"

        MelodicMinor ->
            "3 X 2^X 3"

        HarmonicMinor ->
            "3 3 2^X 141"

        Wholestep ->
            "X X repeat"

        Chromatic ->
            "0123 | 123 repeat"

        MinorPentatonic ->
            "Root - 124 12"

        MajorPentatonic ->
            "1x2412 in one Pos"

        Blues ->
            "Root - 1x234 1x2"

        Mandalorian ->
            "J∆ƒƒ∆ - Ǥ∆ʓ∆ɲ - I∆ɳ"


doublestopPatternToString : Key -> String
doublestopPatternToString key =
    case key of
        Ionian ->
            "m M M m - m M M m"

        Dorian ->
            "M M m m - M M m M"

        Phrygian ->
            "M m m M - M m M M"

        Lydian ->
            "m m M M - m M M m"

        Mixolydian ->
            "m M M m - M M m m"

        Aeolian ->
            "M M m M - M m m M"

        MelodicMinor ->
            "M M m M - m M M M"

        HarmonicMinor ->
            "M M m M - m m M M"

        Wholestep ->
            "m m repeat"

        Chromatic ->
            "M M repeat"

        _ ->
            ""


allIntervals : List Interval
allIntervals =
    [ Sixths, Thirds, Octaves, Fourths, Fifths, Tenths ]


allRanges : List Range
allRanges =
    [ NoEmptyStrings
    , AllEmptyStrings
    , AString
    , DString
    , GString
    , CString
    , NoAString
    , NoADString
    ]


presetToString : Preset -> String
presetToString preset =
    case preset of
        Easy ->
            "EASY"

        All ->
            "ALL"

        None ->
            "NONE"

        Custom ->
            "CUSTOM"


rangeToString : Range -> String
rangeToString range =
    case range of
        NoEmptyStrings ->
            "No Empty Strings"

        AllEmptyStrings ->
            "Empty Strings where possible"

        AString ->
            "Play on A String"

        DString ->
            "Play on D String"

        GString ->
            "Play on G String"

        CString ->
            "Play on C String"

        NoAString ->
            "Go up D String"

        NoADString ->
            "Go up G String"


allBowings : List Bowing
allBowings =
    [ Slured 1
    , Slured 2
    , Slured 3
    , Slured 4
    , Slured 5
    , Slured 6
    , Slured 7
    , Slured 8
    , RepeatedStaccato 1
    , RepeatedStaccato 2
    , RepeatedStaccato 3
    , RepeatedStaccato 4
    , RepeatedStaccato 5
    , RepeatedStaccato 6
    , RepeatedStaccato 7
    , RepeatedStaccato 8
    , RepeatedTenuto 1
    , RepeatedTenuto 2
    , RepeatedTenuto 3
    , RepeatedTenuto 4
    , RepeatedTenuto 5
    , RepeatedTenuto 6
    , RepeatedTenuto 7
    , RepeatedTenuto 8
    , BowStaccato 1
    , BowStaccato 2
    , BowStaccato 3
    , BowStaccato 4
    , BowStaccato 5
    , BowStaccato 6
    , BowStaccato 7
    , BowStaccato 8
    , Sequenced
    , AddTopNote
    , Rhythmed
    ]


initialModel : Encode.Value -> Model
initialModel flags =
    let
        configuration =
            case Decode.decodeValue decodeConfiguration flags of
                Ok config ->
                    config

                Err _ ->
                    { bowings = [ Slured 2, Slured 3, Slured 1, Slured 4 ]
                    , chords = [ Major ]
                    , intervals = []
                    , keys = [ Ionian ]
                    , ranges = []
                    , roots = [ G, C, F ]
                    , topics = [ Scales, Chords ]
                    }
    in
    { elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = True
    , message = Nothing
    , preset = All

    -- selection
    , practiceMode = TimeLimit 5
    , topic =
        List.head configuration.topics
            |> Maybe.withDefault Scales

    --
    -- , topics = [ Scales, Chords, Doublestops ]
    , topics = configuration.topics
    , roots = configuration.roots
    , keys = configuration.keys
    , intervals = configuration.intervals
    , ranges = configuration.ranges
    , bowings = configuration.bowings
    , chords = configuration.chords
    }



-- update


type Msg
    = Tick Time.Posix
    | ToggleTimer
    | ClearTimer
    | KeyPressed String
    | NewExercise
    | NewRootsGenerated (List Root)
    | NewKeysGenerated (List Key)
    | NewIntervalsGenerated (List Interval)
    | NewRangesGenerated (List Range)
    | NewBowingsGenerated (List Bowing)
    | NewChordsGenerated (List Chord)
    | NextTopic
    | ToggleSettings
    | ToggleTopic Topic
    | TogglePracticeMode PracticeMode
    | ToggleRoot Root
    | ToggleChord Chord
    | ToggleKey Key
    | ToggleBowing Bowing
    | ToggleRange Range
    | ToggleInterval Interval
      -- toggle everything for a setting
    | ToggleAllTopics
    | ToggleAllRoots
    | ToggleAllChords
    | ToggleAllKeys
    | ToggleAllBowings
    | ToggleAllRanges
    | ToggleAllIntervals
      -- skip setting
    | SkipTopic
    | SkipRoot
    | SkipChord
    | SkipKey
    | SkipBowing
    | SkipRange
    | SkipInterval
      --
    | ChangePreset Preset
    | PrintConfiguration
    | UpdatedSlider String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newTime =
                    model.elapsedTime + 1

                timeLimitInSeconds =
                    case model.practiceMode of
                        TimeLimit minutes ->
                            minutes * 60

                        ExerciseLimit _ ->
                            0
            in
            if model.isRunning then
                ( { model
                    | elapsedTime = newTime
                    , message =
                        if newTime == timeLimitInSeconds // 3 then
                            Just (Info "Finished 1/3, continue to the next topic if you like" 5)

                        else if newTime == timeLimitInSeconds // 2 then
                            Just (Info "Halftime, keep going!" 5)

                        else if newTime == 2 * (timeLimitInSeconds // 3) then
                            Just (Info "Finished 2/3, continue to the next topic if you like" 5)

                        else if newTime == timeLimitInSeconds then
                            Just (Success "Yay, you're awesome!" 5)

                        else
                            case model.message of
                                Just (Info text time) ->
                                    if time == 0 then
                                        Nothing

                                    else
                                        Just (Info text (time - 1))

                                Just (Error text time) ->
                                    if time == 0 then
                                        Nothing

                                    else
                                        Just (Error text (time - 1))

                                Just (Success text time) ->
                                    if time == 0 then
                                        Nothing

                                    else
                                        Just (Success text (time - 1))

                                Nothing ->
                                    Nothing
                  }
                , Cmd.none
                )

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
            , shuffleEverything model
            )

        ToggleSettings ->
            ( { model
                | showSettings = not model.showSettings
              }
            , Cmd.none
            )

        NextTopic ->
            case model.topics of
                _ :: [] ->
                    ( model, Cmd.none )

                current :: next :: rest ->
                    ( { model
                        | completedExercises = model.completedExercises + 1
                        , topics = next :: rest ++ [ current ]
                      }
                    , shuffleEverything model
                    )

                [] ->
                    ( model, Cmd.none )

        NewKeysGenerated keys ->
            ( { model | keys = keys }, Cmd.none )

        NewRootsGenerated roots ->
            ( { model | roots = roots }, Cmd.none )

        NewBowingsGenerated bowings ->
            ( { model | bowings = bowings }, Cmd.none )

        NewIntervalsGenerated intervals ->
            ( { model | intervals = intervals }, Cmd.none )

        NewChordsGenerated chords ->
            ( { model | chords = chords }, Cmd.none )

        NewRangesGenerated ranges ->
            ( { model | ranges = ranges }, Cmd.none )

        ToggleBowing bowing ->
            ( { model | preset = Custom }
            , shuffleBowings (toggle bowing model.bowings)
            )

        ToggleChord chord ->
            ( { model | preset = Custom }
            , shuffleChords (toggle chord model.chords)
            )

        ToggleRoot root ->
            ( { model | preset = Custom }
            , shuffleRoots (toggle root model.roots)
            )

        ToggleInterval interval ->
            ( { model | preset = Custom }
            , shuffleIntervals (toggle interval model.intervals)
            )

        ToggleRange range ->
            ( { model | preset = Custom }
            , shuffleRanges (toggle range model.ranges)
            )

        ToggleKey key ->
            ( { model | preset = Custom }
            , shuffleKeys (toggle key model.keys)
            )

        TogglePracticeMode practiceMode ->
            ( { model | practiceMode = practiceMode }, Cmd.none )

        ToggleTopic topic ->
            -- let
            --     length =
            --         List.length model.topics
            -- in
            -- ( if length /= 1 then
            --     { model | topics = toggle topic model.topics }
            --   else if length == 1 && not (List.member topic model.topics) then
            --     { model | topics = toggle topic model.topics }
            --   else
            --     model
            ( { model | topics = toggle topic model.topics, preset = Custom }
            , Cmd.none
            )

        ToggleAllTopics ->
            ( { model | topics = toggleList model.topics allTopics, preset = Custom }, Cmd.none )

        ToggleAllRoots ->
            ( { model | roots = toggleList model.roots allRoots, preset = Custom }, Cmd.none )

        ToggleAllIntervals ->
            ( { model | intervals = toggleList model.intervals allIntervals, preset = Custom }, Cmd.none )

        ToggleAllKeys ->
            ( { model | keys = toggleList model.keys allScales, preset = Custom }, Cmd.none )

        ToggleAllRanges ->
            ( { model | ranges = toggleList model.ranges allRanges, preset = Custom }, Cmd.none )

        ToggleAllBowings ->
            ( { model | bowings = toggleList model.bowings allBowings, preset = Custom }, Cmd.none )

        ToggleAllChords ->
            ( { model | chords = toggleList model.chords allChords, preset = Custom }, Cmd.none )

        SkipTopic ->
            ( { model | topics = appendFirstItem model.topics }, Cmd.none )

        SkipInterval ->
            ( { model | intervals = appendFirstItem model.intervals }, Cmd.none )

        SkipRange ->
            ( { model | ranges = appendFirstItem model.ranges }, Cmd.none )

        SkipKey ->
            ( { model | keys = appendFirstItem model.keys }, Cmd.none )

        SkipChord ->
            ( { model | chords = appendFirstItem model.chords }, Cmd.none )

        SkipBowing ->
            ( { model | bowings = appendFirstItem model.bowings }, Cmd.none )

        SkipRoot ->
            ( { model | roots = appendFirstItem model.roots }, Cmd.none )

        ChangePreset preset ->
            let
                newModel =
                    { model | preset = preset } |> applyPreset
            in
            ( newModel, shuffleEverything newModel )

        UpdatedSlider newValue ->
            ( { model
                | practiceMode =
                    case model.practiceMode of
                        TimeLimit _ ->
                            String.toInt newValue
                                |> Maybe.withDefault 15
                                |> TimeLimit

                        ExerciseLimit _ ->
                            String.toInt newValue
                                |> Maybe.withDefault 15
                                |> ExerciseLimit
              }
            , Cmd.none
            )

        PrintConfiguration ->
            let
                cmd =
                    -- in order to build, the following code needs to be commented out
                    Debug.toString
                        { bowings = model.bowings
                        , topics = model.topics
                        , roots = model.roots
                        , keys = model.keys
                        , chords = model.chords
                        , ranges = model.ranges
                        , intervals = model.intervals
                        }
                        |> String.replace "], " "]\n---\n"
                        |> String.replace "{ " ""
                        |> String.replace "}" ""
                        |> printToConsole

                -- and this needds to be commented in
                -- Cmd.none
            in
            ( model, cmd )


appendFirstItem : List a -> List a
appendFirstItem items =
    case items of
        first :: rest ->
            rest ++ [ first ]

        [] ->
            []


applyPreset : Model -> Model
applyPreset model =
    case model.preset of
        Easy ->
            { model
                | bowings = [ Slured 2, Slured 3, Slured 1, Slured 4 ]
                , chords = [ Major ]
                , intervals = []
                , keys = [ Ionian ]
                , ranges = []
                , roots = [ G, C, F ]
                , topics = [ Scales, Chords ]
            }

        All ->
            { model
                | topics = [ Scales, Chords, Doublestops ]
                , roots = allRoots
                , keys = allScales
                , intervals = allIntervals
                , ranges = allRanges
                , bowings = allBowings
                , chords = allChords
            }

        None ->
            { model
                | topics = []
                , roots = []
                , keys = []
                , intervals = []
                , ranges = []
                , bowings = []
                , chords = []
            }

        Custom ->
            model


toggle element list =
    if List.member element list then
        List.filter ((/=) element) list

    else
        element :: list


toggleList : List a -> List a -> List a
toggleList items allItems =
    if List.isEmpty items then
        allItems

    else
        []


clearTimer : Model -> ( Model, Cmd Msg )
clearTimer model =
    ( { model
        | isRunning = False
        , elapsedTime = 0
        , message = Nothing
      }
    , Cmd.none
    )


toggleTimer : Model -> ( Model, Cmd Msg )
toggleTimer model =
    if model.isRunning then
        ( { model | isRunning = False }, Cmd.none )

    else
        ( { model | isRunning = True }, Cmd.none )


shuffleEverything : Model -> Cmd Msg
shuffleEverything model =
    Cmd.batch
        [ shuffleIntervals model.intervals
        , shuffleKeys model.keys
        , shuffleBowings model.bowings
        , shuffleRanges model.ranges
        , shuffleRoots model.roots
        ]


shuffleRoots : List Root -> Cmd Msg
shuffleRoots roots =
    Random.generate NewRootsGenerated (Random.List.shuffle roots)


shuffleRanges : List Range -> Cmd Msg
shuffleRanges ranges =
    Random.generate NewRangesGenerated (Random.List.shuffle ranges)


shuffleKeys : List Key -> Cmd Msg
shuffleKeys keys =
    Random.generate NewKeysGenerated (Random.List.shuffle keys)


shuffleIntervals : List Interval -> Cmd Msg
shuffleIntervals intervals =
    Random.generate NewIntervalsGenerated (Random.List.shuffle intervals)


shuffleBowings : List Bowing -> Cmd Msg
shuffleBowings bowings =
    Random.generate NewBowingsGenerated (Random.List.shuffle bowings)


shuffleChords : List Chord -> Cmd Msg
shuffleChords chords =
    Random.generate NewChordsGenerated (Random.List.shuffle chords)



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
    div [ class "font-mono bg-gray-200 px-5 py-5 min-h-screen flex items-start" ]
        [ selectionContainer model
        , settings model
        ]


selectionContainer : Model -> Html Msg
selectionContainer model =
    div [ class "container bg-gray-200 px-5 py-5 max-w-lg rounded" ]
        [ header model
        , infoBox model.message
        , selection model
        ]


selection : Model -> Html Msg
selection model =
    let
        intervals =
            selectionItem model.intervals intervalToString SkipInterval "Interval: "

        roots =
            selectionItem model.roots rootToString SkipRoot "Root: "

        keys =
            selectionItem model.keys keyToString SkipKey "Key: "

        chords =
            selectionItem model.chords chordToString SkipChord "Chord: "

        ranges =
            selectionItem model.ranges rangeToString SkipRange "Extra challenge: "

        bowings =
            selectionItem model.bowings bowingToString SkipBowing "Bowings: "

        scalePatterns =
            selectionItem model.keys scalePatternToString SkipKey "Scale pattern: "

        doublestopPatterns =
            selectionItem model.keys doublestopPatternToString SkipKey "Doublestop pattern: "

        spacing =
            div [ class "container text-left bg-gray mb-1 p-2" ]
                []
    in
    div [ class "container flex-col mx-auto justify-center p-3 bg-gray-200 px-4 rounded" ]
        ([ selectionItem model.topics (String.toUpper << topicToString) SkipTopic ""
         , spacing
         ]
            ++ (case List.head model.topics of
                    Just Scales ->
                        [ roots
                        , keys
                        , scalePatterns
                        , spacing
                        , bowings
                        , spacing
                        , ranges
                        ]

                    Just Chords ->
                        [ roots
                        , chords
                        , bowings
                        , spacing
                        , ranges
                        ]

                    Just Doublestops ->
                        [ intervals
                        , roots
                        , keys
                        , doublestopPatterns
                        , spacing
                        , bowings
                        , spacing
                        , ranges
                        ]

                    _ ->
                        []
               )
            ++ [ div [ class "container p-3 flex" ]
                    [ button [ class primaryButton, class "flex-auto m-2", onClick NewExercise ] [ text "New exercise" ]
                    , button
                        [ class <|
                            if List.length model.topics < 2 then
                                secondaryButton

                            else
                                primaryButton
                        , class "flex-auto m-2"
                        , onClick NextTopic
                        ]
                        [ text "Next topic" ]
                    ]
               ]
        )


slider : Model -> List (Html Msg)
slider model =
    let
        getValue mode =
            case mode of
                TimeLimit number ->
                    number

                ExerciseLimit number ->
                    number
    in
    [ input
        [ type_ "range"
        , A.min "5"
        , A.max "20"
        , value <| String.fromInt (getValue model.practiceMode)
        , onInput UpdatedSlider
        , class "text-black mr-2 px-2 rounded"
        ]
        []
    , text <| String.fromInt <| getValue model.practiceMode
    ]


infoBox : Maybe Message -> Html msg
infoBox message =
    let
        ( color, content ) =
            case message of
                Just (Info msg _) ->
                    ( "yellow-300", msg )

                Just (Error msg _) ->
                    ( "red-300", msg )

                Just (Success msg _) ->
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


selectionItem : List a -> (a -> String) -> Msg -> String -> Html Msg
selectionItem items toString skip label =
    List.head items
        |> Maybe.map toString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [ class "container text-center bg-gray-200 mb-1 p-2 border-gray-400 border-b-2 rounded select-none" ]
                        [ text "-/-"
                        ]

                else
                    div
                        [ class "container text-left bg-white mb-1 p-2 border-gray-400 border-b-2 rounded select-none"
                        , onClick skip
                        ]
                        [ text label
                        , text string
                        ]
           )


settings : Model -> Html Msg
settings model =
    let
        ( buttonTimeLimit, buttonExercises ) =
            case model.practiceMode of
                TimeLimit _ ->
                    ( buttonActive, buttonPassive )

                ExerciseLimit _ ->
                    ( buttonPassive, buttonActive )
    in
    if model.showSettings then
        div [ class "container bg-gray-200 px-5 py-5 rounded" ]
            [ div [ class "container bg-gray-200 font-mono rounded" ] <|
                [ div [ class "container mx-2" ]
                    [ div [ class "container" ] [ text "Presets" ]
                    , presetButton Easy model
                    , presetButton All model
                    , presetButton None model
                    , presetButton Custom model
                    ]
                , div
                    [ class "container mx-2" ]
                  <|
                    [ div [ class "container" ] [ text "Practice mode" ]
                    , button
                        [ class buttonTimeLimit
                        , onClick (TogglePracticeMode <| TimeLimit 5)
                        ]
                        [ text "Time limit" ]
                    , button
                        [ class buttonExercises
                        , onClick (TogglePracticeMode <| ExerciseLimit 5)
                        ]
                        [ text "Exercise limit" ]
                    ]
                        ++ slider model
                , settingsFor model.topics allTopics topicToString ToggleTopic ToggleAllTopics "Topics"
                , settingsFor model.roots allRoots rootToString ToggleRoot ToggleAllRoots "Roots"
                , settingsFor model.intervals allIntervals intervalToString ToggleInterval ToggleAllIntervals "Intervals"
                , settingsFor model.keys allScales keyToString ToggleKey ToggleAllKeys "Keys"
                , settingsFor model.chords allChords chordToString ToggleChord ToggleAllChords "Chords"

                -- :: showRangeSliderSetting model
                , settingsFor model.bowings allBowings bowingToString ToggleBowing ToggleAllBowings "Bowings"
                , settingsFor model.ranges allRanges rangeToString ToggleRange ToggleAllRanges "Challenges"
                , div [ class "container m-2" ]
                    [ button
                        [ class """bg-yellow-500 hover:bg-yellow-400 cursor-pointer text-white font-bold mr-2 mb-1 px-2 
    border-b-2 border-yellow-700 hover:border-yellow-500 rounded"""
                        , onClick PrintConfiguration
                        ]
                        [ text "EXPORT" ]
                    ]
                ]
            ]

    else
        div [] []


settingsFor : List a -> List a -> (a -> String) -> (a -> Msg) -> Msg -> String -> Html Msg
settingsFor currentItems allItems itemToString toggleSingle toggleAll label =
    div [ class "container m-2" ] <|
        div [ class "container" ] [ button [ onClick toggleAll ] [ text label ] ]
            :: showSetting itemToString allItems currentItems toggleSingle


presetButton : Preset -> Model -> Html Msg
presetButton preset model =
    button
        [ class <|
            if model.preset == preset then
                coloredButton "indigo" 300 400 700

            else
                buttonPassive
        , onClick (ChangePreset preset)
        ]
        [ text <| presetToString preset ]


coloredButton : String -> Int -> Int -> Int -> String
coloredButton color light normal dark =
    "bg-"
        ++ color
        ++ "-"
        ++ String.fromInt normal
        ++ " hover:bg-"
        ++ color
        ++ "-"
        ++ String.fromInt light
        ++ " cursor-pointer text-white"
        ++ " font-bold mr-2 mb-1 px-2 border-b-2 border-"
        ++ color
        ++ "-"
        ++ String.fromInt dark
        ++ " hover:border-"
        ++ color
        ++ "-"
        ++ String.fromInt normal
        ++ " rounded"


buttonActive =
    """bg-green-500 hover:bg-green-400 cursor-pointer text-white font-bold mr-2 mb-1 px-2 
    border-b-2 border-green-700 hover:border-green-500 rounded"""


buttonPassive =
    """bg-gray-500 hover:bg-gray-400 cursor-pointer text-white font-bold mr-2 mb-1 px-2 
    border-b-2 border-gray-700 hover:border-gray-500 rounded"""


showRangeSliderSetting model =
    List.map
        (\element ->
            button
                [ class <|
                    if List.member element model.ranges then
                        buttonActive

                    else
                        buttonPassive
                , onClick (ToggleRange element)
                ]
                [ text <| rangeToString element ]
        )
        allRanges
        ++ slider model


showSetting : (a -> String) -> List a -> List a -> (a -> Msg) -> List (Html Msg)
showSetting toString elements selectedElements msg =
    List.map
        (\element ->
            button
                [ class <|
                    if List.member element selectedElements then
                        buttonActive

                    else
                        buttonPassive
                , onClick (msg element)
                ]
                [ text <| toString element ]
        )
        elements


primaryButton : String
primaryButton =
    """bg-pink-500 hover:bg-pink-400 cursor-pointer text-white 
    font-bold py-2 px-4 border-b-4 border-pink-700 hover:border-pink-500 rounded"""


secondaryButton : String
secondaryButton =
    """bg-gray-500 hover:bg-gray-400 cursor-pointer text-white 
    font-bold py-2 px-4 border-b-4 border-gray-700 hover:border-gray-500 rounded"""


header : Model -> Html Msg
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
            "px-2 mr-2 mb-2 bg-gray-100 rounded border-b-2"

        buttonClass =
            """bg-gray-500 hover:bg-gray-400 cursor-pointer text-white 
            font-bold mr-2 px-2 border-b-2 border-gray-700 hover:border-gray-500 rounded"""
    in
    div [ class "container inline-flex flex flex-row font-mono" ]
        [ div [ class "container flex justify-end items-start" ]
            [ div [ class elementClass ] [ text (practiceModeToString model.practiceMode) ]
            , div [ class elementClass ]
                [ text (toDoubleDigits minutes ++ ":" ++ toDoubleDigits seconds)
                ]
            , button [ class buttonClass, onClick ToggleTimer ]
                [ text <|
                    if model.isRunning then
                        "pause"

                    else
                        "start"
                ]
            , button [ class buttonClass, onClick ClearTimer ] [ text "■" ]
            , button [ class buttonClass, onClick ToggleSettings ] [ text "..." ]
            ]
        ]



-- JSON ENCODE/DECODE


encodeBowing a =
    case a of
        Slured times ->
            Encode.object
                [ ( "kind", Encode.string "Slured" )
                , ( "times", Encode.int times )
                ]

        RepeatedStaccato times ->
            Encode.object
                [ ( "kind", Encode.string "RepeatedStaccato" )
                , ( "times", Encode.int times )
                ]

        RepeatedTenuto times ->
            Encode.object
                [ ( "kind", Encode.string "RepeatedTenuto" )
                , ( "times", Encode.int times )
                ]

        BowStaccato times ->
            Encode.object
                [ ( "kind", Encode.string "BowStaccato" )
                , ( "times", Encode.int times )
                ]

        Sequenced ->
            Encode.object
                [ ( "kind", Encode.string "Sequenced" )
                ]

        AddTopNote ->
            Encode.object
                [ ( "kind", Encode.string "AddTopNote" )
                ]

        Rhythmed ->
            Encode.object
                [ ( "kind", Encode.string "Rhythmed" )
                ]


encodeChord a =
    Encode.string <| chordToString a


encodeConfiguration a =
    Encode.object
        [ ( "topics", Encode.list encodeTopic a.topics )
        , ( "roots", Encode.list encodeRoot a.roots )
        , ( "keys", Encode.list encodeKey a.keys )
        , ( "intervals", Encode.list encodeInterval a.intervals )
        , ( "ranges", Encode.list encodeRange a.ranges )
        , ( "bowings", Encode.list encodeBowing a.bowings )
        , ( "chords", Encode.list encodeChord a.chords )
        ]


encodeInterval interval =
    Encode.string <| intervalToString interval


encodeKey interval =
    Encode.string <| keyToString interval


encodeRange range =
    Encode.string <| rangeToString range


encodeRoot root =
    Encode.string <| rootToString root


encodeTopic topic =
    Encode.string <| topicToString topic


decodeBowing =
    Decode.field "kind" Decode.string |> Decode.andThen decodeBowingHelp


decodeBowingHelp kind =
    case kind of
        "Slured" ->
            Decode.map
                Slured
                (Decode.field "times" Decode.int)

        "RepeatedStaccato" ->
            Decode.map
                RepeatedStaccato
                (Decode.field "times" Decode.int)

        "RepeatedTenuto" ->
            Decode.map
                RepeatedTenuto
                (Decode.field "times" Decode.int)

        "BowStaccato" ->
            Decode.map
                BowStaccato
                (Decode.field "times" Decode.int)

        "Sequenced" ->
            Decode.succeed Sequenced

        "AddTopNote" ->
            Decode.succeed AddTopNote

        "Rhythmed" ->
            Decode.succeed Rhythmed

        other ->
            Decode.fail <| "Unknown constructor for type Bowing: " ++ other


decodeChord =
    let
        recover x =
            case x of
                "Major" ->
                    Decode.succeed Major

                "Minor" ->
                    Decode.succeed Minor

                "Dim" ->
                    Decode.succeed Dim

                "Augm" ->
                    Decode.succeed Aug

                "Sus2" ->
                    Decode.succeed Sus2

                "Sus4" ->
                    Decode.succeed Sus4

                "Maj7" ->
                    Decode.succeed Maj7

                "Min7" ->
                    Decode.succeed Min7

                "Dom7" ->
                    Decode.succeed Dom7

                "MinMaj7" ->
                    Decode.succeed MinMaj7

                "HalfDim7" ->
                    Decode.succeed HalfDim7

                "Dim7" ->
                    Decode.succeed Dim7

                other ->
                    Decode.fail <| "Unknown constructor for type Chord: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeConfiguration =
    Decode.map7
        Configuration
        (Decode.field "topics" (Decode.list decodeTopic))
        (Decode.field "roots" (Decode.list decodeRoot))
        (Decode.field "keys" (Decode.list decodeKey))
        (Decode.field "intervals" (Decode.list decodeInterval))
        (Decode.field "ranges" (Decode.list decodeRange))
        (Decode.field "bowings" (Decode.list decodeBowing))
        (Decode.field "chords" (Decode.list decodeChord))


decodeInterval =
    let
        recover x =
            case x of
                "6ths" ->
                    Decode.succeed Sixths

                "3rds" ->
                    Decode.succeed Thirds

                "8ths" ->
                    Decode.succeed Octaves

                "parallel 4ths" ->
                    Decode.succeed Fourths

                "parallel 5ths" ->
                    Decode.succeed Fifths

                "10ths" ->
                    Decode.succeed Tenths

                other ->
                    Decode.fail <| "Unknown constructor for type Interval: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeKey =
    let
        recover x =
            case x of
                "Major (Ionian)" ->
                    Decode.succeed Ionian

                "Dorian" ->
                    Decode.succeed Dorian

                "Phrygian" ->
                    Decode.succeed Phrygian

                "Lydian" ->
                    Decode.succeed Lydian

                "Mixolydian" ->
                    Decode.succeed Mixolydian

                "Minor (Natural, Aeolian)" ->
                    Decode.succeed Aeolian

                "Mandalorian" ->
                    Decode.succeed Mandalorian

                "Melodic Minor" ->
                    Decode.succeed MelodicMinor

                "Harmonic Minor" ->
                    Decode.succeed HarmonicMinor

                "Major Pentatonic" ->
                    Decode.succeed MajorPentatonic

                "Minor Pentatonic" ->
                    Decode.succeed MinorPentatonic

                "Chromatic" ->
                    Decode.succeed Chromatic

                "Wholestep" ->
                    Decode.succeed Wholestep

                "Blues" ->
                    Decode.succeed Blues

                other ->
                    Decode.fail <| "Unknown constructor for type Key: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeRange =
    let
        recover x =
            case x of
                "No Empty Strings" ->
                    Decode.succeed NoEmptyStrings

                "Empty Strings where possible" ->
                    Decode.succeed AllEmptyStrings

                "Play on A String" ->
                    Decode.succeed AString

                "Play on D String" ->
                    Decode.succeed DString

                "Play on G String" ->
                    Decode.succeed GString

                "Play on C String" ->
                    Decode.succeed CString

                "Go up D String" ->
                    Decode.succeed NoAString

                "Go up G String" ->
                    Decode.succeed NoADString

                other ->
                    Decode.fail <| "Unknown constructor for type Range: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeRoot =
    let
        recover x =
            case x of
                "A" ->
                    Decode.succeed A

                "Bb" ->
                    Decode.succeed Bb

                "B" ->
                    Decode.succeed B

                "C" ->
                    Decode.succeed C

                "C# / Db" ->
                    Decode.succeed Cis

                "D" ->
                    Decode.succeed D

                "Eb / D#" ->
                    Decode.succeed Dis

                "E" ->
                    Decode.succeed E

                "F" ->
                    Decode.succeed F

                "F# / Gb" ->
                    Decode.succeed Fis

                "G" ->
                    Decode.succeed G

                "G# / Ab" ->
                    Decode.succeed Gis

                other ->
                    Decode.fail <| "Unknown constructor for type Root: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeTopic =
    let
        recover x =
            case x of
                "Scales" ->
                    Decode.succeed Scales

                "Chords" ->
                    Decode.succeed Chords

                "Doublestops" ->
                    Decode.succeed Doublestops

                other ->
                    Decode.fail <| "Unknown constructor for type Topic: " ++ other
    in
    Decode.string |> Decode.andThen recover
