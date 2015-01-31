bcodex
======

A command-line utility in Haskell for converting between "encodings". Not character encodings like Unicode or latin1, things like bytes written in hexadecimal or the puzzlehunt classic A=1, B=2 cipher.

It now also supports various text transformations, which can be chained in weird ways.

Naming
======

`codex` is a nice name that sounds like it has to do with codes but is actually a word of its own meaning a sort of book, you know? Plus it sounds cool. But I prefixed a `b` so I don't use up the name for better projects by more professional people. Actually, there are already hundreds of projects called "codex" or some obvious derivative thereof, so hopefully this name is at least sort of unique.

Various example usages:

- `bcodex alpha to numbers`
- `bcodex numbers to alpha`
- `bcodex numbers to hexadecimal`
- `bcodex numbers to base 36`
- `bcodex chars to numbers`
- `bcodex chars to bytes`
- `bcodex chars to base64`
- `bcodex 8 bits to bytes`
- `bcodex 8 bits to chars`
- `bcodex 8 bits to bytes drop spaces`
- `bcodex rot13`
- `bcodex shift 7`
- `bcodex to morse`
- `bcodex morse`
- `bcodex uppercase`

License
=======

MIT
