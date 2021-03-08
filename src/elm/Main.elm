port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Configuration exposing (..)
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes as A exposing (class, max, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Types exposing (..)



-- ports


port printToConsole : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg



-- main


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { elapsedTime : Int
    , completedExercises : Int
    , isRunning : Bool
    , showSettings : Bool
    , message : Maybe Message
    , practiceMode : PracticeMode
    , configuration : Configuration
    , showScalePattern : Bool
    , autoNextExercise : Bool
    , elapsedExerciseTime : Int
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        localInitialModel =
            initialModel flags
    in
    ( localInitialModel
    , Cmd.none
    )


initialModel : Encode.Value -> Model
initialModel flags =
    let
        configuration =
            case Decode.decodeValue decodeConfiguration flags of
                Ok config ->
                    config

                Err _ ->
                    configurationFor Basic
    in
    { elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = False
    , message = Nothing
    , practiceMode = TimeLimit 15
    , configuration = configuration
    , showScalePattern = False
    , autoNextExercise = False
    , elapsedExerciseTime = 0
    }



-- update


type Msg
    = Tick Time.Posix
    | ToggleTimer
    | ClearProgress
    | KeyPressed String
    | NewExercise
      --
    | NewConfigurationGenerated Configuration
    | NewRootsGenerated (List Root)
    | NewScalesGenerated (List Scale)
    | NewIntervalsGenerated (List Interval)
    | NewChallengesGenerated (List Challenge)
    | NewBowingsGenerated (List Bowing)
    | NewChordsGenerated (List Chord)
    | NextTopic
    | SwitchPracticeMode
      --
    | ToggleSettings
    | ToggleTopic Topic
    | ToggleRoot Root
    | ToggleChord Chord
    | ToggleScale Scale
    | ToggleBowing Bowing
    | ToggleChallenge Challenge
    | ToggleInterval Interval
      --
    | ToggleAllTopics
    | ToggleAllRoots
    | ToggleAllChords
    | ToggleAllScales
    | ToggleAllBowings
    | ToggleAllChallenges
    | ToggleAllIntervals
    | ToggleShowScalePattern
    | ToggleAutoNextExercise
      --
    | SkipTopic
    | SkipRoot
    | SkipChord
    | SkipScale
    | SkipBowing
    | SkipChallenge
    | SkipInterval
      --
    | ChangePreset Preset
    | PrintConfiguration
    | UpdatedSlider String


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encodeConfiguration newModel.configuration), cmds ]
    )


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
                    , elapsedExerciseTime =
                        if model.autoNextExercise then
                            model.elapsedExerciseTime + 1

                        else
                            0
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

        ClearProgress ->
            clearProgress model

        KeyPressed key ->
            if key == " " then
                ( { model
                    | completedExercises = model.completedExercises + 1
                  }
                , shuffleConfig NewConfigurationGenerated model.configuration
                )

            else
                ( model, Cmd.none )

        NewExercise ->
            ( { model
                | completedExercises = model.completedExercises + 1
              }
            , shuffleConfig NewConfigurationGenerated model.configuration
            )

        ToggleSettings ->
            ( { model
                | showSettings = not model.showSettings
              }
            , Cmd.none
            )

        NextTopic ->
            let
                newConfiguration =
                    nextTopic model.configuration
            in
            ( { model
                | completedExercises = model.completedExercises + 1
                , configuration = newConfiguration
              }
            , shuffleConfig NewConfigurationGenerated newConfiguration
            )

        NewScalesGenerated scales ->
            ( { model | configuration = updateScales scales model.configuration }, Cmd.none )

        NewRootsGenerated roots ->
            ( { model | configuration = updateRoots roots model.configuration }, Cmd.none )

        NewBowingsGenerated bowings ->
            ( { model | configuration = updateBowings bowings model.configuration }, Cmd.none )

        NewIntervalsGenerated intervals ->
            ( { model | configuration = updateIntervals intervals model.configuration }, Cmd.none )

        NewChordsGenerated chords ->
            ( { model | configuration = updateChords chords model.configuration }, Cmd.none )

        NewConfigurationGenerated configuration ->
            ( { model | configuration = configuration }, Cmd.none )

        NewChallengesGenerated challenges ->
            ( { model | configuration = updateChallenges challenges model.configuration }, Cmd.none )

        ToggleBowing bowing ->
            ( model, shuffleBowings NewBowingsGenerated (toggleBowing bowing model.configuration) )
                |> setToCustomPreset

        ToggleChord chord ->
            ( model, shuffleChords NewChordsGenerated (toggleChord chord model.configuration) )
                |> setToCustomPreset

        ToggleRoot root ->
            ( model, shuffleRoots NewRootsGenerated (toggleRoot root model.configuration) )
                |> setToCustomPreset

        ToggleInterval interval ->
            ( model, shuffleIntervals NewIntervalsGenerated (toggleInterval interval model.configuration) )
                |> setToCustomPreset

        ToggleChallenge challenge ->
            ( model, shuffleChallenges NewChallengesGenerated (toggleChallenge challenge model.configuration) )
                |> setToCustomPreset

        ToggleScale scale ->
            ( model, shuffleScales NewScalesGenerated (toggleScale scale model.configuration) )
                |> setToCustomPreset

        ToggleShowScalePattern ->
            ( { model | showScalePattern = not model.showScalePattern }, Cmd.none )

        ToggleAutoNextExercise ->
            ( { model | autoNextExercise = not model.autoNextExercise }, Cmd.none )

        SwitchPracticeMode ->
            let
                newPracticeMode =
                    case model.practiceMode of
                        TimeLimit n ->
                            ExerciseLimit n

                        ExerciseLimit n ->
                            TimeLimit n
            in
            ( { model | practiceMode = newPracticeMode }, Cmd.none )

        ToggleTopic topic ->
            ( { model | configuration = toggleTopic topic model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllTopics ->
            ( { model | configuration = toggleAll getTopics allTopics updateTopics model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllRoots ->
            ( { model | configuration = toggleAll getRoots allRoots updateRoots model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllIntervals ->
            ( { model | configuration = toggleAll getIntervals allIntervals updateIntervals model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllScales ->
            ( { model | configuration = toggleAll getScales allScales updateScales model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllChallenges ->
            ( { model | configuration = toggleAll getChallenges allChallenges updateChallenges model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllBowings ->
            ( { model | configuration = toggleAll getBowings allBowings updateBowings model.configuration }, Cmd.none )
                |> setToCustomPreset

        ToggleAllChords ->
            ( { model | configuration = toggleAll getChords allChords updateChords model.configuration }, Cmd.none )
                |> setToCustomPreset

        SkipTopic ->
            ( { model | configuration = nextTopic model.configuration }, Cmd.none )

        SkipInterval ->
            ( { model | configuration = nextInterval model.configuration }, Cmd.none )

        SkipChallenge ->
            ( { model | configuration = nextChallenge model.configuration }, Cmd.none )

        SkipScale ->
            ( { model | configuration = nextScale model.configuration }, Cmd.none )

        SkipChord ->
            ( { model | configuration = nextChord model.configuration }, Cmd.none )

        SkipBowing ->
            ( { model | configuration = nextBowing model.configuration }, Cmd.none )

        SkipRoot ->
            ( { model | configuration = nextRoot model.configuration }, Cmd.none )

        ChangePreset preset ->
            let
                newConfiguration =
                    configurationFor preset
            in
            ( { model | configuration = newConfiguration }
            , shuffleConfig NewConfigurationGenerated newConfiguration
            )

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
                    -- Debug.toString
                    --     model.configuration
                    --     |> String.replace "], " "]\n---\n"
                    --     |> String.replace "{ " ""
                    --     |> String.replace "}" ""
                    --     |> printToConsole

                -- and this needds to be commented in
                    Cmd.none
            in
            ( model, cmd )



-- toggleNextExercise (model, cmd) =
--     let
--         (newModel, newCmd) = if model.elapsedExerciseTime > 60 then
--                                     ({model | elapsedExerciseTime = 0}, ToggleAutoNextExercise)
--                              else (model, Cmd.none)
--     in
--     (newModel, newCmd)


setToCustomPreset : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setToCustomPreset ( model, cmds ) =
    ( { model | configuration = updatePreset Custom model.configuration }, cmds )


clearProgress : Model -> ( Model, Cmd Msg )
clearProgress model =
    ( { model
        | isRunning = False
        , elapsedTime = 0
        , completedExercises = 0
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
        configuration =
            model.configuration

        intervals =
            selectionItem configuration.intervals intervalToString SkipInterval "Interval: "

        roots =
            selectionItem configuration.roots rootToString SkipRoot "Root: "

        scales =
            selectionItem configuration.scales scaleToString SkipScale "Scale: "

        chords =
            selectionItem configuration.chords chordToString SkipChord "Chord: "

        challenges =
            selectionItem configuration.challenges challengeToString SkipChallenge "Extra challenge: "

        bowings =
            selectionItem configuration.bowings bowingToString SkipBowing "Bowings: "

        scalePatterns =
            if model.showScalePattern then
                selectionItem configuration.scales scalePatternToString SkipScale "Scale pattern: "

            else
                div [ class "container text-center bg-gray-200 mb-1 p-2 border-gray-400 border-b-2 rounded select-none" ]
                    [ text "-/-" ]

        doublestopPatterns =
            selectionItem configuration.scales doublestopPatternToString SkipScale "Doublestop pattern: "

        spacing =
            div [ class "container text-left bg-gray mb-1 p-2" ]
                []
    in
    div [ class "container flex-col mx-auto justify-center p-3 bg-gray-200 px-4 rounded" ]
        ([ selectionItem configuration.topics (String.toUpper << topicToString) SkipTopic ""
         , spacing
         ]
            ++ (case List.head configuration.topics of
                    Just Scales ->
                        [ roots
                        , scales
                        , spacing
                        , scalePatterns
                        , bowings
                        , spacing
                        , challenges
                        ]

                    Just Chords ->
                        [ roots
                        , chords
                        , bowings
                        , spacing
                        , challenges
                        ]

                    Just Doublestops ->
                        [ intervals
                        , roots
                        , scales
                        , spacing
                        , doublestopPatterns
                        , bowings
                        , spacing
                        , challenges
                        ]

                    _ ->
                        []
               )
            ++ [ div [ class "container p-3 flex" ]
                    [ button
                        [ class <|
                            if model.autoNextExercise then
                                primaryButton

                            else
                                buttonPassive
                        , class "m-2"
                        , onClick ToggleAutoNextExercise
                        ]
                        [ text "🚔" ]
                    , button
                        [ class <| coloredButton "yellow" 400 500 800, class "flex-auto m-2", onClick NewExercise ]
                        [ text "Done" ]
                    , button
                        [ class <|
                            if List.length configuration.topics < 2 then
                                buttonPassive

                            else
                                primaryButton
                        , class "flex-end m-2"
                        , onClick NextTopic
                        ]
                        [ text "💃" ]
                    ]
               ]
        )


practiceModeSlider : Model -> List (Html Msg)
practiceModeSlider model =
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
        , A.max "60"
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
                        [ text "-/-" ]

                else
                    div
                        [ class "container text-left bg-white mb-1 p-2 border-gray-400 border-b-2 rounded select-none"
                        , onClick skip
                        ]
                        [ text label, text string ]
           )


settings : Model -> Html Msg
settings model =
    let
        configuration =
            model.configuration
    in
    if model.showSettings then
        div [ class "container bg-gray-200 px-5 py-5 rounded" ]
            [ div [ class "container bg-gray-200 font-mono rounded" ] <|
                [ div [ class "container rounded flex" ]
                    [ div [ class "container" ]
                        [ presets model
                        , Html.br [] []
                        , practiceMode model
                        , settingsFor configuration.topics
                            allTopics
                            topicToString
                            ToggleTopic
                            ToggleAllTopics
                            "Topics"
                            4
                        ]
                    , div [ class "container pl-10" ]
                        [ settingsFor configuration.roots
                            allRoots
                            rootToString
                            ToggleRoot
                            ToggleAllRoots
                            "Roots"
                            3
                        , settingsFor configuration.intervals
                            allIntervals
                            intervalToString
                            ToggleInterval
                            ToggleAllIntervals
                            "Intervals"
                            3
                        ]
                    ]
                , div [ class "container flex" ]
                    [ div [ class "container" ]
                        [ settingsFor configuration.scales
                            allScales
                            scaleToString
                            ToggleScale
                            ToggleAllScales
                            "Scales"
                            1
                        , button
                            [ onClick ToggleShowScalePattern ]
                            [ text "👀" ]
                        , settingsFor configuration.challenges
                            allChallenges
                            challengeToString
                            ToggleChallenge
                            ToggleAllChallenges
                            "Challenges"
                            4
                        ]
                    , div [ class "container pl-10" ]
                        [ settingsFor configuration.chords
                            allChords
                            chordToString
                            ToggleChord
                            ToggleAllChords
                            "Chords"
                            2
                        , settingsFor configuration.bowings
                            allBowings
                            bowingToString
                            ToggleBowing
                            ToggleAllBowings
                            "Bowings"
                            2
                        ]
                    ]

                -- :: showRangeSliderSetting model
                ]
            ]

    else
        div [] []


practiceMode : Model -> Html Msg
practiceMode model =
    let
        ( buttonTimeLimit, buttonExercises ) =
            case model.practiceMode of
                TimeLimit _ ->
                    ( buttonActive, buttonPassive )

                ExerciseLimit _ ->
                    ( buttonPassive, buttonActive )
    in
    div
        [ class "container m-6" ]
    <|
        [ div [ class "container font-bold" ] [ text "Practice mode" ]
        , Html.br [] []
        , button
            [ class buttonTimeLimit
            , onClick SwitchPracticeMode
            ]
            [ text "Time limit" ]
        , button
            [ class buttonExercises
            , onClick SwitchPracticeMode
            ]
            [ text "Exercise limit" ]
        ]
            ++ practiceModeSlider model


presets : Model -> Html Msg
presets model =
    div [ class "container m-6" ]
        [ div [ class "container font-bold" ] [ text "Presets" ]
        , Html.br [] []
        , presetButton Basic model
        , presetButton All model
        , presetButton None model
        , presetButton Custom model
        , Html.br [] []
        , div [ class "container m-2" ]
            [ button
                [ class <| coloredButton "yellow" 400 500 700
                , onClick PrintConfiguration
                ]
                [ text "EXPORT" ]
            ]
        ]


settingsFor : List a -> List a -> (a -> String) -> (a -> Msg) -> Msg -> String -> Int -> Html Msg
settingsFor currentItems allItems itemToString toggleSingle toggleAllMsg label divideAt =
    div [ class "container m-6" ] <|
        div [ class "container" ] [ button [ class "font-bold", onClick toggleAllMsg ] [ text label ] ]
            :: showSetting divideAt itemToString allItems currentItems toggleSingle


presetButton : Preset -> Model -> Html Msg
presetButton preset model =
    button
        [ class <|
            if getPreset model.configuration == preset then
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
        ++ " cursor-pointer text-black"
        ++ " font-regular mr-2 mb-2 px-2 border-b-2 border-"
        ++ String.fromInt (String.toInt color |> Maybe.withDefault 200 |> (-) 100)
        ++ "-"
        ++ String.fromInt dark
        ++ " hover:border-"
        ++ color
        ++ "-"
        ++ String.fromInt normal
        ++ " rounded"


buttonActive : String
buttonActive =
    coloredButton "green" 500 400 800


buttonPassive : String
buttonPassive =
    coloredButton "gray" 400 200 700


primaryButton : String
primaryButton =
    coloredButton "indigo" 300 400 700


showRangeSliderSetting model =
    let
        configuration =
            model.configuration
    in
    List.map
        (\element ->
            button
                [ class <|
                    if List.member element configuration.challenges then
                        buttonActive

                    else
                        buttonPassive
                , onClick (ToggleChallenge element)
                ]
                [ text <| challengeToString element ]
        )
        allChallenges
        ++ practiceModeSlider model


showSetting : Int -> (a -> String) -> List a -> List a -> (a -> Msg) -> List (Html Msg)
showSetting divideAt toString elements selectedElements msg =
    List.indexedMap
        (\i element ->
            if remainderBy divideAt i == 0 then
                [ Html.br [] []
                , button
                    [ class <|
                        if List.member element selectedElements then
                            buttonActive

                        else
                            buttonPassive
                    , onClick (msg element)
                    ]
                    [ text <| toString element ]
                ]

            else
                [ button
                    [ class <|
                        if List.member element selectedElements then
                            buttonActive

                        else
                            buttonPassive
                    , onClick (msg element)
                    ]
                    [ text <| toString element ]
                ]
        )
        elements
        |> List.concat


header : Model -> Html Msg
header model =
    let
        elementClass =
            "px-2 mr-2 mb-2 bg-gray-100 rounded border-b-2"
    in
    div [ class "container inline-flex flex flex-row font-mono" ]
        [ div [ class "container flex justify-end items-start" ]
            [ button [ class elementClass, onClick SwitchPracticeMode ] [ text (practiceModeToString model.practiceMode) ]
            , progressBar model
            , button [ class buttonPassive, onClick ToggleTimer ]
                [ text <|
                    if model.isRunning then
                        "pause"

                    else
                        "start"
                ]
            , button [ class buttonPassive, onClick ClearProgress ] [ text "■" ]
            , button [ class buttonPassive, onClick ToggleSettings ] [ text "..." ]
            ]
        ]


progressBar : Model -> Html Msg
progressBar model =
    let
        ( maximum, value_ ) =
            (case model.practiceMode of
                TimeLimit time ->
                    ( time * 60, model.elapsedTime )

                ExerciseLimit exercises ->
                    ( exercises, model.completedExercises )
            )
                |> Tuple.mapBoth String.fromInt String.fromInt
    in
    progress [ A.max <| maximum, value value_, class "mt-1 mr-2" ] []



-- JSON ENCODE/DECODE


encodeBowing : Bowing -> Encode.Value
encodeBowing bowing =
    case bowing of
        Slured times ->
            Encode.object
                [ ( "kind", Encode.string "Slured" )
                , ( "times", Encode.int times )
                ]

        Repeated times ->
            Encode.object
                [ ( "kind", Encode.string "Repeated" )
                , ( "times", Encode.int times )
                ]


encodeChord : Chord -> Encode.Value
encodeChord chord =
    chordToString chord |> Encode.string


encodeConfiguration : Configuration -> Encode.Value
encodeConfiguration config =
    Encode.object
        [ ( "topics", Encode.list encodeTopic config.topics )
        , ( "roots", Encode.list encodeRoot config.roots )
        , ( "scales", Encode.list encodeScale config.scales )
        , ( "intervals", Encode.list encodeInterval config.intervals )
        , ( "challenges", Encode.list encodeChallenge config.challenges )
        , ( "bowings", Encode.list encodeBowing config.bowings )
        , ( "chords", Encode.list encodeChord config.chords )
        , ( "preset", encodePreset config.preset )
        ]


encodeInterval : Interval -> Encode.Value
encodeInterval interval =
    Encode.string <| intervalToString interval


encodeScale : Scale -> Encode.Value
encodeScale scale =
    Encode.string <| scaleToString scale


encodeChallenge : Challenge -> Encode.Value
encodeChallenge challenge =
    Encode.string <| challengeToString challenge


encodeRoot : Root -> Encode.Value
encodeRoot root =
    Encode.string <| rootToString root


encodeTopic : Topic -> Encode.Value
encodeTopic topic =
    Encode.string <| topicToString topic


encodePreset : Preset -> Encode.Value
encodePreset preset =
    Encode.string <| presetToString preset


decodeBowing : Decode.Decoder Bowing
decodeBowing =
    Decode.field "kind" Decode.string |> Decode.andThen decodeBowingHelp


decodeBowingHelp : String -> Decode.Decoder Bowing
decodeBowingHelp kind =
    case kind of
        "Slured" ->
            Decode.map
                Slured
                (Decode.field "times" Decode.int)

        "Repeated" ->
            Decode.map
                Repeated
                (Decode.field "times" Decode.int)

        other ->
            Decode.fail <| "Unknown constructor for type Bowing: " ++ other


decodeChord : Decode.Decoder Chord
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


decodeConfiguration : Decode.Decoder Configuration
decodeConfiguration =
    Decode.map8
        Configuration
        (Decode.field "topics" (Decode.list decodeTopic))
        (Decode.field "roots" (Decode.list decodeRoot))
        (Decode.field "scales" (Decode.list decodeScale))
        (Decode.field "intervals" (Decode.list decodeInterval))
        (Decode.field "challenges" (Decode.list decodeChallenge))
        (Decode.field "bowings" (Decode.list decodeBowing))
        (Decode.field "chords" (Decode.list decodeChord))
        (Decode.field "preset" decodePreset)


decodeInterval : Decode.Decoder Interval
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

                "4ths" ->
                    Decode.succeed Fourths

                "5ths" ->
                    Decode.succeed Fifths

                other ->
                    Decode.fail <| "Unknown constructor for type Interval: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeScale : Decode.Decoder Scale
decodeScale =
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


decodeChallenge : Decode.Decoder Challenge
decodeChallenge =
    let
        recover x =
            case x of
                "Play on A String" ->
                    Decode.succeed AString

                "Play on D String" ->
                    Decode.succeed DString

                other ->
                    Decode.fail <| "Unknown constructor for type Challenge: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeRoot : Decode.Decoder Root
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


decodeTopic : Decode.Decoder Topic
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


decodePreset : Decode.Decoder Preset
decodePreset =
    let
        recover x =
            case x of
                "Basic" ->
                    Decode.succeed Basic

                "All" ->
                    Decode.succeed All

                "None" ->
                    Decode.succeed None

                "Custom" ->
                    Decode.succeed Custom

                other ->
                    Decode.fail <| "Unknown constructor for type Topic: " ++ other
    in
    Decode.string |> Decode.andThen recover
