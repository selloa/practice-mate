port module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Configuration exposing (..)
import Html exposing (Html, button, div, input, label, progress, text)
import Html.Attributes as A exposing (checked, class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Time
import Types exposing (..)



-- config


buttonSize =
    26



-- ports


port printToConsole : String -> Cmd msg


port setStorage : Encode.Value -> Cmd msg


port setLink : Encode.Value -> Cmd msg



-- main


main : Program Flags Model Msg
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
    , autoNextTimeInMinutes : Int
    , elapsedExerciseTime : Int
    , flags : Flags
    , error : String
    }


type alias Flags =
    Encode.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.none
    )


initialModel : Flags -> Model
initialModel flags =
    let
        ( storedConfiguration, error ) =
            case Decode.decodeValue decodeConfiguration flags of
                Ok config ->
                    ( config, "" )

                Err err ->
                    ( configurationFor Basic, Debug.log "" <| Decode.errorToString err )
    in
    { elapsedTime = 0
    , completedExercises = 0
    , isRunning = False
    , showSettings = False
    , message = Nothing
    , practiceMode = TimeLimit 30
    , configuration = storedConfiguration
    , showScalePattern = False
    , autoNextExercise = False
    , autoNextTimeInMinutes = 1
    , elapsedExerciseTime = 0
    , flags = flags
    , error = error
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
    | PreviousTopic
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
    | UpdatedAutoNextSlider String
    | AddConfigToLink


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

                newExerciseTime =
                    if model.autoNextExercise then
                        model.elapsedExerciseTime + 1

                    else
                        model.elapsedExerciseTime

                timeLimitInSeconds =
                    case model.practiceMode of
                        TimeLimit minutes ->
                            minutes * 60
            in
            if model.isRunning then
                ( { model
                    | isRunning = timeLimitInSeconds /= newTime
                    , elapsedTime = newTime
                    , elapsedExerciseTime =
                        if newExerciseTime > 59 then
                            0

                        else
                            newExerciseTime
                    , message =
                        if newTime == timeLimitInSeconds then
                            Just (Success "Yay, you're awesome! 🎻" 10)

                        else if newTime > 2 * (timeLimitInSeconds // 3) then
                            Just (Info "Two thirds, almost done! 💃")

                        else if newTime > timeLimitInSeconds // 2 then
                            Just (Info "Halftime, keep going! 👯\u{200D}♂️")

                        else if newTime > timeLimitInSeconds // 3 then
                            Just (Info "One third, amazing! \u{1F973}")

                        else
                            Nothing
                  }
                , if newExerciseTime > 59 then
                    shuffleConfig NewConfigurationGenerated model.configuration

                  else
                    Cmd.none
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

            else if key == "Enter" then
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

            else
                ( model, Cmd.none )

        NewExercise ->
            ( { model
                | completedExercises = model.completedExercises + 1
                , elapsedExerciseTime = 0
              }
            , shuffleConfig NewConfigurationGenerated model.configuration
            )

        ToggleSettings ->
            ( { model
                | showSettings = not model.showSettings
                , isRunning = False
              }
            , Cmd.none
            )

        NextTopic ->
            let
                newConfiguration =
                    nextTopic model.configuration
            in
            ( { model
                | configuration = newConfiguration
                , elapsedExerciseTime = 0
              }
            , shuffleConfig NewConfigurationGenerated newConfiguration
            )

        PreviousTopic ->
            let
                newConfiguration =
                    previousTopic model.configuration
            in
            ( { model
                | configuration = newConfiguration
                , elapsedExerciseTime = 0
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

        UpdatedAutoNextSlider newValue ->
            ( { model
                | autoNextTimeInMinutes =
                    String.toInt newValue
                        |> Maybe.withDefault 2
              }
            , Cmd.none
            )

        UpdatedSlider newValue ->
            ( { model
                | practiceMode =
                    case model.practiceMode of
                        TimeLimit _ ->
                            String.toInt newValue
                                |> Maybe.withDefault 15
                                |> TimeLimit
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

        AddConfigToLink ->
            ( model, setLink <| encodeConfiguration model.configuration )



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
        , elapsedExerciseTime = 0
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
    div [ class "font-serif bg-gray-100 md:px-5 md:py-5 min-h-screen w-screen flex flex-col md:flex-row items-start md:justify-center" ] <|
        if model.showSettings then
            [ button [ class buttonPassive, class "ml-1 md:ml-0 mt-1 md:my-20 p-1", onClick ToggleSettings ]
                [ Filled.arrow_back_ios_new buttonSize Inherit ]
            , settings model
            ]

        else
            [ selectionContainer model ]


selectionContainer : Model -> Html Msg
selectionContainer model =
    div [ class "container bg-gray-200 px-5 py-5 max-w-lg h-screen sm:h-auto rounded w-screen" ]
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
            intervalSelection configuration

        roots =
            rootSelection configuration

        scales =
            scaleSelection configuration

        chords =
            chordSelection configuration

        challenges =
            selectionItem configuration.challenges challengeToString SkipChallenge "Challenge "

        bowings =
            bowingSelection configuration

        scalePatterns =
            if model.showScalePattern then
                selectionItem configuration.scales scalePatternToString SkipScale ""

            else
                div [] []

        doublestopPatterns =
            selectionItem configuration.scales doublestopPatternToString SkipScale ""

        spacing =
            div [ class "container bg-gray mb-1 p-2" ]
                []

        showTopic =
            -- div [ class "container flex justify-center p-6" ]
            -- [ case topic of
            --     Scales ->
            --         Filled.auto_graph 40 Inherit
            --     Intervals ->
            --         Filled.stacked_line_chart 40 Inherit
            --     Chords ->
            --         Filled.scatter_plot 40 Inherit
            -- ]
            List.head configuration.topics
                |> Maybe.map topicToString
                |> Maybe.withDefault ""
                |> (\t ->
                        div
                            [ class "italic container text-center text-4xl mb-1 p-2 rounded select-none"
                            ]
                            [ text t
                            ]
                   )
    in
    div [ class "container flex-col mx-auto text-center justify-center p-3 bg-gray-300 px-4 rounded" ]
        ([ showTopic
         , spacing
         ]
            ++ (case List.head configuration.topics of
                    Just Scales ->
                        [ roots
                        , div [ class "inline p-2" ] []
                        , scales
                        , spacing
                        , spacing
                        , div [ class "font-sans" ] [ scalePatterns ]
                        , bowings
                        , challenges
                        ]

                    Just Chords ->
                        [ roots
                        , div [ class "inline p-1" ] []
                        , chords
                        , spacing
                        , spacing
                        , bowings
                        , challenges
                        ]

                    Just Intervals ->
                        [ intervals
                        , div [ class "inline m-4" ] [ text "in" ]
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
                        [ class <| coloredButton "orange" 300 400 800
                        , class "flex-end m-2"
                        , onClick PreviousTopic
                        ]
                        [ Filled.navigate_before buttonSize Inherit ]
                    , button
                        [ class <| coloredButton "yellow" 400 500 800
                        , class "display-block flex-auto ml-2 px-16 sm:px-32"
                        , onClick NewExercise
                        ]
                        --  [ text "refresh" ]
                        [ Filled.auto_fix_high 20 Inherit ]
                    , button
                        [ class <| coloredButton "indigo" 300 200 800
                        , class "flex-end m-2"
                        , onClick NextTopic
                        ]
                        [ Filled.navigate_next buttonSize Inherit ]
                    ]
               ]
        )


infoBox : Maybe Message -> Html msg
infoBox message =
    let
        ( color, content ) =
            case message of
                Just (Info msg) ->
                    ( "indigo-300", msg )

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
            [ div [ class <| "container text-left bg-" ++ color ++ " mt-5 mb-1 p-2" ]
                [ text content
                ]
            ]


bowingSelection : Configuration -> Html Msg
bowingSelection configuration =
    List.head configuration.bowings
        |> Maybe.map bowingToString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [] []

                else
                    button
                        [ class "bg-white p-1 mb-10 border-gray-400 border-t-2 rounded select-none"
                        , onClick SkipBowing
                        ]
                        [ div [ class "inline flex" ] [ Filled.music_note 20 Inherit, text string ] ]
           )


intervalSelection : Configuration -> Html Msg
intervalSelection configuration =
    List.head configuration.intervals
        |> Maybe.map intervalToString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [] []

                else
                    button
                        [ class "text-left text-2xl bg-white p-1 px-4 border-gray-400 border-t-2 rounded select-none"
                        , onClick SkipInterval
                        ]
                        [ text string ]
           )


chordSelection : Configuration -> Html Msg
chordSelection configuration =
    List.head configuration.chords
        |> Maybe.map chordToString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [] []

                else
                    button
                        [ class "text-left text-2xl bg-white p-1 px-4 border-gray-400 border-t-2 rounded select-none"
                        , onClick SkipChord
                        ]
                        [ text string ]
           )


scaleSelection : Configuration -> Html Msg
scaleSelection configuration =
    List.head configuration.scales
        |> Maybe.map scaleToString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [] []

                else
                    button
                        [ class "text-left text-2xl bg-white p-1 px-4 border-gray-400 border-t-2 rounded select-none"
                        , onClick SkipScale
                        ]
                        [ text string ]
           )


rootSelection : Configuration -> Html Msg
rootSelection configuration =
    List.head configuration.roots
        |> Maybe.map rootToString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [] []

                else
                    button
                        [ class "text-left text-2xl bg-white p-1 border-gray-400 px-4 border-t-2 rounded select-none"
                        , onClick SkipRoot
                        ]
                        [ text string ]
           )


selectionItem : List a -> (a -> String) -> Msg -> String -> Html Msg
selectionItem items toString skip label =
    List.head items
        |> Maybe.map toString
        |> Maybe.withDefault ""
        |> (\string ->
                if String.isEmpty string then
                    div [ class "container mb-20" ] []

                else
                    div [ class "container mb-10" ]
                        [ text label
                        , button
                            [ class "text-left bg-white p-1 border-gray-400 px-4 border-t-2 rounded select-none"
                            , onClick skip
                            ]
                            [ text string ]
                        ]
           )


settings : Model -> Html Msg
settings model =
    let
        configuration =
            model.configuration
    in
    if model.showSettings then
        div [ class "container flex flex-wrap bg-gray-200 px-5 rounded" ]
            [ Html.h1 [ class "text-5xl" ] [ text "Settings" ]
            , div [ class "container bg-gray-200 rounded m-6 " ]
                [ button [ onClick AddConfigToLink ] [ text "get link" ] ]
            , div [ class "container bg-gray-200 rounded" ] <|
                [ presets model
                , practiceMode model
                , autoTimer model
                , settingsFor configuration.topics
                    allTopics
                    topicToString
                    ToggleTopic
                    ToggleAllTopics
                    "Topics"
                    4
                , settingsFor configuration.roots
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
                , div [ class "container" ]
                    [ let
                        scaleSettings =
                            div [ class "container m-6" ] <|
                                div [ class "container" ]
                                    [ button [ class "font-bold", onClick ToggleAllScales ]
                                        [ text "Scales" ]
                                    , Html.br [] []
                                    , label [ class "checkbox" ]
                                        [ input
                                            [ class "m-3"
                                            , type_ "checkbox"
                                            , checked model.showScalePattern
                                            , onClick ToggleShowScalePattern
                                            ]
                                            []
                                        , text "patterns"
                                        ]
                                    , button
                                        [ class "ml-2", onClick ToggleShowScalePattern ]
                                        [ text <|
                                            if model.showScalePattern then
                                                "\u{1F92B}"

                                            else
                                                "👀"
                                        ]
                                    ]
                                    :: showSetting 1 scaleToString allScales configuration.scales ToggleScale
                      in
                      scaleSettings
                    , settingsFor configuration.chords
                        allChords
                        chordToString
                        ToggleChord
                        ToggleAllChords
                        "Chords"
                        2
                    , settingsFor
                        (configuration.bowings
                            |> List.filter
                                (\b ->
                                    case b of
                                        Slurred _ ->
                                            True

                                        Repeated _ ->
                                            False
                                )
                        )
                        allBowings
                        bowingToString
                        ToggleBowing
                        ToggleAllBowings
                        "Bowings"
                        4
                    , settingsFor configuration.challenges
                        allChallenges
                        challengeToString
                        ToggleChallenge
                        ToggleAllChallenges
                        "Challenges"
                        4
                    ]

                -- :: showRangeSliderSetting model
                ]
            ]

    else
        div [] []


practiceMode : Model -> Html Msg
practiceMode model =
    div
        [ class "container m-6" ]
    <|
        [ div [ class "container font-bold" ] [ text "Practice time" ]
        , Html.br [] []
        ]
            ++ practiceModeSlider model


practiceModeSlider : Model -> List (Html Msg)
practiceModeSlider model =
    let
        getValue mode =
            case mode of
                TimeLimit number ->
                    number
    in
    [ input
        [ type_ "range"
        , A.min "1"
        , A.max "60"
        , value <| String.fromInt (getValue model.practiceMode)
        , onInput UpdatedSlider
        , class "text-black mr-2 ml-1 rounded"
        ]
        []
    , text <| (String.fromInt <| getValue model.practiceMode) ++ " min"
    ]


autoTimer model =
    div
        [ class "container m-6" ]
    <|
        [ label [ class "checkbox p-2" ]
            [ input
                [ class ""
                , type_ "checkbox"
                , checked model.autoNextExercise
                , onClick ToggleAutoNextExercise
                ]
                []
            , div [ class "pl-2 inline" ] [ text "auto show next ex. after" ]
            ]
        ]
            ++ autoTimerSlider model


autoTimerSlider : Model -> List (Html Msg)
autoTimerSlider model =
    let
        getValue mode =
            case mode of
                TimeLimit number ->
                    number
    in
    [ Html.br [] []
    , input
        [ type_ "range"
        , A.min "1"
        , A.max "5"
        , value <| String.fromInt model.autoNextTimeInMinutes
        , onInput UpdatedAutoNextSlider
        , class "text-black m-2 rounded"
        , A.disabled <| not model.autoNextExercise
        ]
        []
    , text <| String.fromInt model.autoNextTimeInMinutes ++ " min"
    ]


presets : Model -> Html Msg
presets model =
    div [ class "container m-6" ]
        [ div [ class "container flex font-bold" ] [ text "Presets" ]
        , Html.br [] []
        , presetButton Basic model
        , presetButton All model
        , presetButton None model
        , presetButton Custom model

        -- , Html.br [] []
        -- , button
        --     [ class <| coloredButton "yellow" 400 500 700
        --     , onClick PrintConfiguration
        --     ]
        --     [ text "EXPORT" ]
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
        ++ " font-regular mr-2 mb-2 px-2 border--2 border-"
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
    div [ class "container inline-flex flex flex-row" ]
        [ div [ class "container flex justify-start items-start" ]
            [ progressBar model
            , button [ class elementClass, onClick ClearProgress ]
                [ Filled.skip_previous buttonSize Inherit ]
            , button [ class elementClass, onClick ToggleTimer ]
                [ if model.isRunning then
                    Filled.pause buttonSize Inherit

                  else
                    Filled.play_arrow buttonSize Inherit
                ]

            --  , button
            --     [ class <|
            --         if model.autoNextExercise then
            --             coloredButton "gray" 400 500 800
            --         else
            --             coloredButton "gray" 300 400 800
            --     , onClick ToggleAutoNextExercise
            --     ]
            --     [ Filled.timelapse buttonSize Inherit]
            ]
        , button [ class elementClass, class "flex-grow", onClick ToggleSettings ]
            [ Filled.tune buttonSize Inherit
            ]
        ]


progressBar : Model -> Html Msg
progressBar model =
    let
        ( maximum, value_ ) =
            (case model.practiceMode of
                TimeLimit time ->
                    ( time * 60, model.elapsedTime )
            )
                |> Tuple.mapBoth String.fromInt String.fromInt
    in
    progress [ A.max <| maximum, value value_, class "mt-1 mr-2" ] []



-- JSON ENCODE/DECODE


encodeBowing : Bowing -> Encode.Value
encodeBowing bowing =
    case bowing of
        Slurred times ->
            Encode.object
                [ ( "kind", Encode.string "Slurred" )
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
    scaleToString scale
        |> String.replace " " ""
        |> Encode.string


encodeChallenge : Challenge -> Encode.Value
encodeChallenge challenge =
    Encode.string <| challengeToString challenge


encodeRoot : Root -> Encode.Value
encodeRoot root =
    rootToString root
        |> String.replace "♭" "b"
        |> String.replace "♯" "#"
        |> Encode.string


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
        "Slurred" ->
            Decode.map
                Slurred
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
        (Decode.field "topics" (Decode.list decodeTopic |> fallbackTo [Scales]))
        (Decode.field "roots" (Decode.list decodeRoot |> fallbackTo allRoots))
        (Decode.field "scales" (Decode.list decodeScale |> fallbackTo []))
        (Decode.field "intervals" (Decode.list decodeInterval |> fallbackTo []))
        (Decode.field "challenges" (Decode.list decodeChallenge |> fallbackTo []))
        (Decode.field "bowings" (Decode.list decodeBowing |> fallbackTo []))
        (Decode.field "chords" (Decode.list decodeChord |> fallbackTo []))
        (Decode.field "preset" decodePreset)


fallbackTo : List a -> Decode.Decoder (List a) -> Decode.Decoder (List a)
fallbackTo fallback decoder  =
    Decode.maybe decoder
        |> Decode.map (Maybe.withDefault fallback)


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
                "Major" ->
                    Decode.succeed Ionian

                "Dorian" ->
                    Decode.succeed Dorian

                "Phrygian" ->
                    Decode.succeed Phrygian

                "Lydian" ->
                    Decode.succeed Lydian

                "Mixolydian" ->
                    Decode.succeed Mixolydian

                "Minor" ->
                    Decode.succeed Aeolian

                "Mandalorian" ->
                    Decode.succeed Mandalorian

                "MelodicMinor" ->
                    Decode.succeed MelodicMinor

                "HarmonicMinor" ->
                    Decode.succeed HarmonicMinor

                "MajorPentatonic" ->
                    Decode.succeed MajorPentatonic

                "MinorPentatonic" ->
                    Decode.succeed MinorPentatonic

                "Chromatic" ->
                    Decode.succeed Chromatic

                "Wholestep" ->
                    Decode.succeed Wholestep

                "Blues" ->
                    Decode.succeed Blues

                "Ionian" ->
                    Decode.succeed Ionian

                "Aeolian" ->
                    Decode.succeed Aeolian

                other ->
                    Decode.fail <| "Unknown constructor for type Key: " ++ other
    in
    Decode.string |> Decode.andThen recover


decodeChallenge : Decode.Decoder Challenge
decodeChallenge =
    let
        recover x =
            case x of
                "A String" ->
                    Decode.succeed AString

                "D String" ->
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

                "Db" ->
                    Decode.succeed Cis

                "D" ->
                    Decode.succeed D

                "Eb" ->
                    Decode.succeed Dis

                "E" ->
                    Decode.succeed E

                "F" ->
                    Decode.succeed F

                "F#" ->
                    Decode.succeed Fis

                "G" ->
                    Decode.succeed G

                "Ab" ->
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

                "Intervals" ->
                    Decode.succeed Intervals

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
