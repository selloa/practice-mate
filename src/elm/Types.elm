module Types exposing (Bowing(..), Challenge(..), Chord(..), Interval(..), Message(..), PracticeMode(..), Preset(..), Root(..), Scale(..), Topic(..), allBowings, allChallenges, allChords, allIntervals, allRoots, allScales, allTopics, bowingToString, challengeToString, chordToString, doublestopPatternToString, intervalToString, practiceModeToString, practiceModeToStringWithoutNumber, presetToString, rootToString, scalePatternToString, scaleToString, topicToString)


type PracticeMode
    = TimeLimit Int


type Topic
    = Scales
    | Chords
    | Intervals


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


type Scale
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


type Challenge
    = AString
    | DString


type Interval
    = Sixths
    | Thirds
    | Octaves
    | Fourths
    | Fifths


type Message
    = Info String
    | Success String Int
    | Error String Int


type Bowing
    = Slured Int
    | Repeated Int


type Preset
    = Basic
    | All
    | None
    | Custom


allRoots : List Root
allRoots =
    [ A, B, Bb, C, Cis, D, Dis, E, F, Fis, G, Gis ]


allTopics : List Topic
allTopics =
    [ Scales, Chords, Intervals ]


rootToString : Root -> String
rootToString root =
    case root of
        A ->
            "A"

        Bb ->
            "B♭"

        B ->
            "B"

        C ->
            "C"

        Cis ->
            "D♭"

        D ->
            "D"

        Dis ->
            "E♭"

        E ->
            "E"

        F ->
            "F"

        Fis ->
            "F♯"

        G ->
            "G"

        Gis ->
            "A♭"


allScales : List Scale
allScales =
    [ Ionian
    , Aeolian
    , MelodicMinor
    , HarmonicMinor
    , Dorian
    , Phrygian
    , Lydian
    , Mixolydian
    , MajorPentatonic
    , MinorPentatonic
    , Chromatic
    , Wholestep
    , Blues
    , Mandalorian
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
        TimeLimit _ ->
            "⏲️ "



--++ String.fromInt exercises


practiceModeToStringWithoutNumber : PracticeMode -> String
practiceModeToStringWithoutNumber mode =
    case mode of
        TimeLimit _ ->
            "Time limit"


topicToString : Topic -> String
topicToString topic =
    case topic of
        Scales ->
            "Scales"

        Chords ->
            "Chords"

        Intervals ->
            "Intervals"


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


scaleToString : Scale -> String
scaleToString scale =
    case scale of
        Ionian ->
            "Major"

        Dorian ->
            "Dorian"

        Phrygian ->
            "Phrygian"

        Lydian ->
            "Lydian"

        Mixolydian ->
            "Mixolydian"

        Aeolian ->
            "Minor"

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
            String.fromInt n

        Repeated n ->
            String.fromInt n


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


scalePatternToString : Scale -> String
scalePatternToString scale =
    case scale of
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


doublestopPatternToString : Scale -> String
doublestopPatternToString scale =
    case scale of
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
    [ Sixths, Thirds, Octaves, Fourths, Fifths ]


allChallenges : List Challenge
allChallenges =
    [ AString
    , DString
    ]


presetToString : Preset -> String
presetToString preset =
    case preset of
        Basic ->
            "Basic"

        All ->
            "All"

        None ->
            "None"

        Custom ->
            "Custom"


challengeToString : Challenge -> String
challengeToString challenge =
    case challenge of
        AString ->
            "A String"

        DString ->
            "D String"


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
    , Slured 9
    , Slured 10
    , Slured 11
    , Slured 12
    , Repeated 1
    , Repeated 2
    , Repeated 3
    , Repeated 4
    , Repeated 5
    , Repeated 6
    , Repeated 7
    , Repeated 8
    , Repeated 9
    , Repeated 10
    , Repeated 11
    , Repeated 12
    ]
