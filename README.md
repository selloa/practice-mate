# practice-mate

a practice session companion for musicians. helps you structure and track your practice time with customizable exercises.

## what it does

practice-mate is a web app built with elm that helps musicians organize their practice sessions. it's designed to be simple, distraction-free, and focused on what matters: getting better at your instrument.

### core features

- **practice timer** - set time limits and track your progress
- **exercise randomization** - never get stuck in a rut with randomized practice material
- **customizable configurations** - choose what you want to practice
- **progress tracking** - see how many exercises you've completed
- **auto-advance** - optionally move to the next exercise automatically

## practice topics

practice-mate supports three main practice areas:

### scales
- major, minor, melodic minor, harmonic minor
- pentatonic scales (major & minor)
- church modes (ionian, dorian, phrygian, lydian, mixolydian, aeolian)
- chromatic, whole step, blues scales
- mandalorian scale

### chords
- basic triads (major, minor, diminished, augmented)
- suspended chords (sus2, sus4)
- seventh chords (maj7, min7, dom7, minmaj7, half-dim7, dim7)

### intervals
- thirds, fourths, fifths, sixths, octaves

## configuration options

### presets
- **basic** - good starting point with common scales and chords
- **all** - everything enabled for comprehensive practice
- **none** - blank slate for custom configuration
- **custom** - your own mix of topics

### customization
- choose specific roots (a, bb, b, c, c#, d, d#, e, f, f#, g, g#)
- select which scales, chords, and intervals to practice
- configure bowing patterns (slurred, repeated)
- set string challenges (a, d, g, c strings)

## getting started

### development
```bash
# install dependencies
npm install

# start development server
npm run dev

# build for production
npm run build
```

### usage
1. open the app in your browser
2. configure your practice session (or use a preset)
3. set your time limit
4. start practicing!
5. the app will randomly present exercises based on your configuration

## tech stack

- **elm** - functional programming for the frontend
- **parcel** - bundling and development server
- **tailwind css** - styling
- **material icons** - ui elements

## project structure

```
src/
├── elm/
│   ├── main.elm          # main application logic
│   ├── types.elm         # data types and enums
│   └── configuration.elm # practice session configuration
├── css/                  # stylesheets
├── js/                   # javascript entry point
└── index.html           # main html file
```

## why "practice-mate"?

named after the practice companion that helps you stay focused and make the most of your practice time. no distractions, just music.

---

built with ❤️ for musicians who want to practice smarter, not harder.
