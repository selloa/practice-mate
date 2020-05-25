module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type PracticeMode
    = TimeLimit Int
    | ExerciseLimit Int


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


type Range
    = OneOctave
    | TwoOctaves


type Pattern
    = Slur Int
    | Repeat Int
    | Staccato Int


type alias Model =
    { practiceMode : PracticeMode
    , topic : Topic
    , root : Root
    , range : Maybe Range
    , pattern : Maybe Pattern
    }


initialModel : Model
initialModel =
    { practiceMode = TimeLimit 900
    , topic = Scales
    , root = C
    , range = Nothing
    , pattern = Nothing
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick NoOp ] [ text "+1" ]
        , button [ onClick NoOp ] [ text "-1" ]
        , div [] [ text <| Debug.toString model ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
