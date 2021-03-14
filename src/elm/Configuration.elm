module Configuration exposing
    ( Configuration
    , configurationFor
    , flip
    , getBowings
    , getChallenges
    , getChords
    , getIntervals
    , getPreset
    , getRoots
    , getScales
    , getTopics
    , next
    , nextBowing
    , nextChallenge
    , nextChord
    , nextInterval
    , nextRoot
    , nextScale
    , nextTopic
    , previousTopic
    , shuffleBowings
    , shuffleChallenges
    , shuffleChords
    , shuffleConfig
    , shuffleIntervals
    , shuffleList
    , shuffleRoots
    , shuffleScales
    , toggle
    , toggleAll
    , toggleBowing
    , toggleChallenge
    , toggleChord
    , toggleInterval
    , toggleList
    , toggleRoot
    , toggleScale
    , toggleTopic
    , updateBowings
    , updateChallenges
    , updateChords
    , updateIntervals
    , updatePreset
    , updateRoots
    , updateScales
    , updateTopics
    )

import Random
import Random.Extra
import Random.List
import Types exposing (..)


type alias Configuration =
    { topics : List Topic
    , roots : List Root
    , scales : List Scale
    , intervals : List Interval
    , challenges : List Challenge
    , bowings : List Bowing
    , chords : List Chord
    , preset : Preset
    }


configurationFor : Preset -> Configuration
configurationFor preset =
    case preset of
        None ->
            { topics = []
            , roots = []
            , scales = []
            , intervals = []
            , challenges = []
            , bowings = []
            , chords = []
            , preset = None
            }

        Basic ->
            { bowings = [ Slurred 1, Slurred 2, Slurred 3, Slurred 4, Slurred 5, Slurred 6, Slurred 7, Slurred 8 ]
            , chords = [ Major, Minor ]
            , intervals = [ Sixths, Thirds, Octaves ]
            , scales = [ Ionian, Aeolian, MelodicMinor, HarmonicMinor ]
            , challenges = []
            , roots = [ A, Bb, Dis, C, D, F, G ]
            , topics = [ Scales, Chords ]
            , preset = Basic
            }

        All ->
            { topics = [ Scales, Chords, Intervals ]
            , roots = allRoots
            , scales = allScales
            , intervals = allIntervals
            , challenges = allChallenges
            , bowings = allBowings
            , chords = allChords
            , preset = All
            }

        Custom ->
            { topics = []
            , roots = []
            , scales = []
            , intervals = []
            , bowings = []
            , challenges = []
            , chords = []
            , preset = Custom
            }


next : List a -> List a
next elements =
    case elements of
        first :: second :: rest ->
            second :: rest ++ [ first ]

        _ ->
            elements


previous : List a -> List a
previous elements =
    case elements of
        first :: second :: third :: [] ->
            [ third, first, second ]

        first :: second :: [] ->
            [ second, first ]

        _ ->
            elements


nextTopic : Configuration -> Configuration
nextTopic configuration =
    { configuration | topics = next configuration.topics }


previousTopic : Configuration -> Configuration
previousTopic configuration =
    { configuration | topics = previous configuration.topics }


nextScale : Configuration -> Configuration
nextScale configuration =
    { configuration | scales = next configuration.scales }


nextBowing : Configuration -> Configuration
nextBowing configuration =
    { configuration | bowings = next configuration.bowings }


nextInterval : Configuration -> Configuration
nextInterval configuration =
    { configuration | intervals = next configuration.intervals }


nextChord : Configuration -> Configuration
nextChord configuration =
    { configuration | chords = next configuration.chords }


nextRoot : Configuration -> Configuration
nextRoot configuration =
    { configuration | roots = next configuration.roots }


nextChallenge : Configuration -> Configuration
nextChallenge configuration =
    { configuration | challenges = next configuration.challenges }


updateScales : List Scale -> Configuration -> Configuration
updateScales scales configuration =
    { configuration | scales = scales }


updateRoots : List Root -> Configuration -> Configuration
updateRoots roots configuration =
    { configuration | roots = roots }


updateBowings : List Bowing -> Configuration -> Configuration
updateBowings bowings configuration =
    { configuration | bowings = bowings }


updatePreset : Preset -> Configuration -> Configuration
updatePreset preset configuration =
    { configuration | preset = preset }


updateChords : List Chord -> Configuration -> Configuration
updateChords chords configuration =
    { configuration | chords = chords }


updateTopics : List Topic -> Configuration -> Configuration
updateTopics topics configuration =
    { configuration | topics = topics }


updateChallenges : List Challenge -> Configuration -> Configuration
updateChallenges challenges configuration =
    { configuration | challenges = challenges }


updateIntervals : List Interval -> Configuration -> Configuration
updateIntervals intervals configuration =
    { configuration | intervals = intervals }


toggleBowing : Bowing -> Configuration -> Configuration
toggleBowing bowing configuration =
    { configuration | bowings = toggle bowing configuration.bowings }


toggleChord : Chord -> Configuration -> Configuration
toggleChord chord configuration =
    { configuration | chords = toggle chord configuration.chords }


toggleTopic : Topic -> Configuration -> Configuration
toggleTopic topic configuration =
    { configuration | topics = toggle topic configuration.topics }


toggleScale : Scale -> Configuration -> Configuration
toggleScale scale configuration =
    { configuration | scales = toggle scale configuration.scales }


toggleInterval : Interval -> Configuration -> Configuration
toggleInterval interval configuration =
    { configuration | intervals = toggle interval configuration.intervals }


toggleChallenge : Challenge -> Configuration -> Configuration
toggleChallenge challenge configuration =
    { configuration | challenges = toggle challenge configuration.challenges }


toggleRoot : Root -> Configuration -> Configuration
toggleRoot root configuration =
    { configuration | roots = toggle root configuration.roots }


shuffleList : (List a -> msg) -> List a -> Cmd msg
shuffleList toMsg items =
    Random.generate toMsg (Random.List.shuffle items)


shuffleBowings : (List Bowing -> msg) -> Configuration -> Cmd msg
shuffleBowings msg configuration =
    shuffleList msg configuration.bowings


shuffleChallenges : (List Challenge -> msg) -> Configuration -> Cmd msg
shuffleChallenges msg configuration =
    shuffleList msg configuration.challenges


shuffleScales : (List Scale -> msg) -> Configuration -> Cmd msg
shuffleScales msg configuration =
    shuffleList msg configuration.scales


shuffleIntervals : (List Interval -> msg) -> Configuration -> Cmd msg
shuffleIntervals msg configuration =
    shuffleList msg configuration.intervals


shuffleRoots : (List Root -> msg) -> Configuration -> Cmd msg
shuffleRoots msg configuration =
    shuffleList msg configuration.roots


shuffleChords : (List Chord -> msg) -> Configuration -> Cmd msg
shuffleChords msg configuration =
    shuffleList msg configuration.chords


getTopics : Configuration -> List Topic
getTopics =
    .topics


getRoots : Configuration -> List Root
getRoots =
    .roots


getChords : Configuration -> List Chord
getChords =
    .chords


getIntervals : Configuration -> List Interval
getIntervals =
    .intervals


getBowings : Configuration -> List Bowing
getBowings =
    .bowings


getChallenges : Configuration -> List Challenge
getChallenges =
    .challenges


getScales : Configuration -> List Scale
getScales =
    .scales


getPreset : Configuration -> Preset
getPreset =
    .preset


toggleAll : (Configuration -> List a) -> List a -> (List a -> Configuration -> Configuration) -> Configuration -> Configuration
toggleAll getter allElements setter configuration =
    configuration
        |> getter
        |> toggleList allElements
        |> flip setter configuration


shuffleConfig : (Configuration -> msg) -> Configuration -> Cmd msg
shuffleConfig toMsg config =
    Random.Extra.map6 Configuration
        (Random.constant config.topics)
        (Random.List.shuffle config.roots)
        (Random.List.shuffle config.scales)
        (Random.List.shuffle config.intervals)
        (Random.List.shuffle config.challenges)
        (Random.List.shuffle config.bowings)
        |> Random.Extra.andMap (Random.List.shuffle config.chords)
        |> Random.Extra.andMap (Random.constant config.preset)
        |> Random.generate toMsg


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


toggle : a -> List a -> List a
toggle element list =
    if List.member element list then
        List.filter ((/=) element) list

    else
        element :: list


toggleList : List a -> List a -> List a
toggleList allItems items =
    if List.isEmpty items then
        allItems

    else
        []
