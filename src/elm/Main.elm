port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Attribute, Html, button, div, h1, input, text)
import Html.Attributes as A exposing (class, max, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Random
import Random.List
import Time



-- ports


port printToConsole : String -> Cmd msg



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
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


type Chord
    = Major
    | Minor
    | Dim
    | Augm
    | Sus2
    | Sus4


type Range
    = FreeRange
    | NoEmptyStrings
    | AllEmptyStrings
    | OnlyAString
    | OnlyDString
    | OnlyGString
    | OnlyCString
    | NoAString
    | NoADString


type Interval
    = Sixths
    | Thirds
    | Octaves
    | Fourths
    | Fifths


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, shuffleEverything initialModel )


allRoots : List Root
allRoots =
    [ A, B, Bb, C, Cis, D, Dis, E, F, Fis, G, Gis ]


allTopics : List Topic
allTopics =
    [ Scales, Chords, Doublestops ]


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
            "F# / Gb"

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
    ]


allChords : List Chord
allChords =
    [ Major, Minor, Dim, Augm, Sus2, Sus4 ]


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

        Augm ->
            "Augm"

        Sus2 ->
            "Sus2"

        Sus4 ->
            "Sus4"


keyToString : Key -> String
keyToString key =
    case key of
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
            "4ths"

        Fifths ->
            "5ths"


fingeringToString : Key -> String
fingeringToString key =
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

        _ ->
            ""


allIntervals : List Interval
allIntervals =
    [ Sixths, Thirds, Octaves, Fourths, Fifths ]


allRanges : List Range
allRanges =
    [ FreeRange
    , NoEmptyStrings
    , AllEmptyStrings
    , OnlyAString
    , OnlyDString
    , OnlyGString
    , OnlyCString
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


rangeToString : Range -> String
rangeToString range =
    case range of
        FreeRange ->
            "Free Range"

        NoEmptyStrings ->
            "No Empty Strings"

        AllEmptyStrings ->
            "Empty Strings where possible"

        OnlyAString ->
            "Play only on A String"

        OnlyDString ->
            "Play only on D String"

        OnlyGString ->
            "Play only on G String"

        OnlyCString ->
            "Play only on C String"

        NoAString ->
            "Don't play on A String"

        NoADString ->
            "Don't play on A or D String"


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


initialModel : Model
initialModel =
    { elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = True
    , message = Nothing
    , preset = All

    -- selection
    , practiceMode = TimeLimit 5
    , topic = Scales

    --
    , topics = [ Scales, Chords, Doublestops ]
    , roots = allRoots
    , keys = allScales
    , intervals = allIntervals
    , ranges = allRanges
    , bowings = allBowings
    , chords = allChords
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
            ( model
            , shuffleBowings (toggle bowing model.bowings)
            )

        ToggleChord chord ->
            ( model
            , shuffleChords (toggle chord model.chords)
            )

        ToggleRoot root ->
            ( model
            , shuffleRoots (toggle root model.roots)
            )

        ToggleInterval interval ->
            ( model
            , shuffleIntervals (toggle interval model.intervals)
            )

        ToggleRange range ->
            ( model
            , shuffleRanges (toggle range model.ranges)
            )

        ToggleKey key ->
            ( model
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
            ( { model | topics = toggle topic model.topics }
            , Cmd.none
            )

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
                , topics = [ Chords, Scales ]
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


toggle element list =
    if List.member element list then
        List.filter ((/=) element) list

    else
        element :: list


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
    div [ class "font-mono bg-gray-200 px-10 py-5 min-h-screen flex items-start" ]
        [ selectionContainer model
        , settings model
        ]


selectionContainer : Model -> Html Msg
selectionContainer model =
    div [ class "container bg-gray-600 px-5 py-5 my-10 max-w-lg rounded" ]
        [ header model
        , infoBox model.message
        , selection model
        ]


selection : Model -> Html Msg
selection model =
    let
        { topics, ranges, bowings, roots, intervals, keys } =
            model
    in
    div [ class "container flex-col mx-auto justify-center p-3 bg-gray-200 px-4 rounded" ]
        [ selectionItem topics (String.toUpper << topicToString) ""
        , div [ class "container text-left bg-gray mb-1 p-2" ]
            []
        , case List.head topics of
            Just Doublestops ->
                selectionItem intervals intervalToString "Interval: "

            _ ->
                div [ class "hidden" ] []
        , selectionItem roots rootToString "Root: "
        , selectionItem keys keyToString "Key: "
        , selectionItem keys fingeringToString "Fingering: "
        , selectionItem bowings bowingToString "Bowing: "
        , selectionItem ranges rangeToString "Range: "
        , div [ class "container p-3 flex" ]
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


selectionItem : List a -> (a -> String) -> String -> Html msg
selectionItem items toString label =
    List.head items
        |> Maybe.map toString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [ class "container text-center bg-gray-200 mb-1 p-2 border-gray-400 border-b-2 rounded" ]
                        [ text "-/-"
                        ]

                else
                    div [ class "container text-left bg-white mb-1 p-2 border-gray-400 border-b-2 rounded" ]
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
        div [ class "container bg-gray-600 px-5 py-5 m-10 rounded" ]
            [ div [ class "container bg-gray-200 font-mono" ] <|
                [ div [ class "container mx-2" ]
                    [ div [ class "container" ] [ text "Presets" ]
                    , presetButton Easy model
                    , presetButton All model
                    , presetButton None model
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
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Topics" ]
                        :: showSetting topicToString allTopics model.topics ToggleTopic
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Roots" ]
                        :: showSetting rootToString allRoots model.roots ToggleRoot
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Interval" ]
                        :: showSetting intervalToString allIntervals model.intervals ToggleInterval
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Key" ]
                        :: showSetting keyToString allScales model.keys ToggleKey
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Chord" ]
                        :: showSetting chordToString allChords model.chords ToggleChord
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Bowings" ]
                        :: showSetting bowingToString allBowings model.bowings ToggleBowing
                , div [ class "container m-2" ] <|
                    div [ class "container" ] [ text "Ranges" ]
                        :: showSetting rangeToString allRanges model.ranges ToggleRange
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


presetButton : Preset -> Model -> Html Msg
presetButton preset model =
    button
        [ class <|
            if model.preset == preset then
                buttonActive

            else
                buttonPassive
        , onClick (ChangePreset preset)
        ]
        [ text <| presetToString preset ]


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
