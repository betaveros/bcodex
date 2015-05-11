bcodex
======

A command-line utility in Haskell for converting between "encodings". Not character encodings like Unicode or latin1, things like bytes written in hexadecimal or the puzzlehunt classic A=1, B=2 cipher.

It now also supports various text transformations, which can be chained in weird ways.

Naming
======

`codex` is a nice name that sounds like it has to do with codes but is actually a word of its own meaning a sort of book, you know? Plus it sounds cool. But I prefixed a `b` so I don't use up the name for better projects by more professional people. Actually, there are already hundreds of projects called "codex" or some obvious derivative thereof, so hopefully this name is at least sort of unique.

Various example usages:

```
$ bcodex rot13 # inverse: bcodex rot13
Shapgvbany cebtenzzvat pbzovarf gur syrkvovyvgl naq cbjre bs nofgenpg zngurzngvpf jvgu gur vaghvgvir pynevgl bs nofgenpg zngurzngvpf.
Functional programming combines the flexibility and power of abstract mathematics with the intuitive clarity of abstract mathematics.

$ bcodex base64 to chars # inverse: bcodex chars to base64
eFF1YXNhcjogV2UgYXJlIGNvb3BlcmF0aW5nIHdpdGggeW91LCB5b3UncmUganVzdCBub3QgYXdhcmUgdGhhdCB5b3VyIGdvYWwgaXMgbGVhcm5pbmcgSGFza2VsbA==
xQuasar: We are cooperating with you, you're just not aware that your goal is learning Haskell

$ bcodex morse # inverse: to morse
.-. . -.-. ..- .-. ... .. --- -. / .. ... / - .... . / -... .-. . .- -.- ..-. .- ... - / --- ..-. / -.-. .... .- -- .--. .. --- -. ... .-.-.-
recursion is the breakfast of champions.

$ bcodex 5 bits to alpha # inverse: alpha to 5 bits
00100011110111010100000010111010100010001001001111100000111101101011111001010000010000100111010001010001101111011011000010101101000010110010100111010001000001011100101000000011010000101010011010010111010000010101110110010111110101001000111110100010000000110100
dontanthropomorphizecomputerstheyhateitwhenyoudothat

$ bcodex numbers to alpha # inverse: alpha to numbers
23 5  1 4 8 5 18 5  20 15  20 8 5  22 1 12 21 5 19  15 6  20 8 5  12 1 13 2 4 1  11 14 9 7 8 20. 19 20 18 5 14 7 20 8, 16 21 18 9 20 25, 1 14 4  12 1 26 9 14 5 19 19.
we adhere to the values of the lambda knight. strength, purity, and laziness.
```

(Quote sources: [xkcd 1270](https://xkcd.com/1270/), ["Trolling #haskell"](https://gist.github.com/quchen/5280339), ["Write Haskell as fast as C"](https://github.com/bitemyapp/learnhaskell/blob/master/write_haskell_as_fast_as_c.md), unknown, [<hae> on #haskell](http://ircbrowse.net/day/haskell/2015/02/15?id=20090966&timestamp=1423967586#t1423967586). Okay I'm just really bored.)

Other usages:

- `bcodex numbers to hexadecimal`
- `bcodex numbers to base 36`
- `bcodex chars to numbers`
- `bcodex chars to bytes`
- `bcodex 8 bits to bytes`
- `bcodex 8 bits to chars`
- `bcodex 8 bits to bytes drop spaces`
- `bcodex shift 7`
- `bcodex uppercase`

License
=======

MIT
