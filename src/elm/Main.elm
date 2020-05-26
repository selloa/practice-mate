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


type
    Key
    -- chords
    = Major
    | Minor
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


type Interval
    = Sixths
    | Thirds
    | Octaves
    | Fourths
    | Fiths


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



-- Model


type alias Model =
    { practiceMode : PracticeMode
    , topic : Topic
    , root : Root
    , key : Key
    , interval : Interval
    , range : Range
    , bowing : Bowing
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


allScales : List Key
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


allChords : List Key
allChords =
    [ Major, Minor, Dim, Augm, Sus2, Sus4 ]


practiceModeToString : PracticeMode -> String
practiceModeToString mode =
    case mode of
        TimeLimit duration ->
            "⏲️ " ++ String.fromInt duration

        ExerciseLimit exercises ->
            "Exercise (" ++ String.fromInt exercises ++ ")"


topicToString : Topic -> String
topicToString topic =
    case topic of
        Scales ->
            "Scales"

        Chords ->
            "Chords"

        Doublestops ->
            "Doublestops"


keyToString : Key -> String
keyToString mode =
    case mode of
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

        Fiths ->
            "5ths"


allIntervals : List Interval
allIntervals =
    [ Sixths, Thirds, Octaves, Fourths, Fiths ]


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
    { practiceMode = TimeLimit 1
    , topic = Scales
    , root = C
    , key = Ionian
    , interval = Thirds
    , range = OneOctave 1
    , bowing = RepeatedStaccato 1
    , elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = False
    , message = Nothing
    }



-- update


type Msg
    = Tick Time.Posix
    | ToggleTimer
    | ClearTimer
    | KeyPressed String
    | NewExercise
    | NewRootGenerated ( Maybe Root, List Root )
    | NewKeyGenerated ( Maybe Key, List Key )
    | NewIntervalGenerated ( Maybe Interval, List Interval )
    | NewRangeGenerated ( Maybe Range, List Range )
    | NewBowingGenerated ( Maybe Bowing, List Bowing )
    | NextTopic
    | ToggleSettings


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

        NewKeyGenerated ( maybeKey, _ ) ->
            case maybeKey of
                Just key ->
                    ( { model | key = key }, Cmd.none )

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

        NewBowingGenerated ( maybeBowing, _ ) ->
            case maybeBowing of
                Just bowing ->
                    ( { model | bowing = bowing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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


generateEverything topic =
    Cmd.batch
        [ generateInterval
        , generateMode topic
        , generateBowing
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
    Random.generate NewKeyGenerated (Random.List.choose source)


generateInterval : Cmd Msg
generateInterval =
    Random.generate NewIntervalGenerated (Random.List.choose allIntervals)


generateBowing : Cmd Msg
generateBowing =
    Random.generate NewBowingGenerated (Random.List.choose allBowings)



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
    div [ class "container font-mono mx-auto bg-gray-200 px-5 py-5 my-10 max-w-lg" ]
        [ header model
        , infoBox model.message
        , selection model
        , settings model
        ]


selection model =
    let
        { practiceMode, topic, range, bowing, root, interval, key } =
            model
    in
    div [ class "container flex-col mx-auto justify-center p-3 bg-gray-300 px-4" ]
        [ -- selectionItem practiceMode practiceModeToString "Practice mode: "
          selectionItem topic (String.toUpper << topicToString) ""
        , div [ class "container text-left bg-gray mb-1 p-2" ]
            []
        , selectionItem root rootToString "Root: "
        , if topic == Doublestops then
            selectionItem interval intervalToString "Interval: "

          else
            div [ class "hidden" ] []
        , selectionItem key keyToString "Key: "
        , selectionItem range rangeToString "Range: "
        , selectionItem bowing bowingToString "Bowings: "
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
                Just (Info msg duration) ->
                    ( "yellow-300", msg )

                Just (Error msg duration) ->
                    ( "red-300", msg )

                Just (Success msg duration) ->
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
    div [ class "container text-left bg-white mb-1 p-2 border-gray-400 border-b-2 rounded" ]
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
            "px-2 mr-2 mb-2 bg-gray-100 rounded border-b-2"

        buttonClass =
            """bg-gray-500 hover:bg-gray-400 cursor-pointer text-white 
            font-bold mr-2 px-2 border-b-2 border-gray-700 hover:border-gray-500 rounded"""
    in
    div [ class "container inline-flex flex flex-row font-mono" ]
        -- [ h1 [ class "text-4xl font-sans" ] [ text "✔︎❒✘❍🎵" ]
        [ h1 [ class elementClass, class "font-sans" ] [ text "❒" ]
        , div [ class "container flex justify-end items-start" ]
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
            , div [ class "px-2 mb-2 bg-gray-100 border-b-2 rounded" ]
                [ text ("✔ " ++ String.fromInt model.completedExercises)
                ]
            ]
        ]
