-- file Spec.hs
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Bcodex
import Text.Bcodex.Utils
import Data.Char
import Control.Arrow (right)

main :: IO ()
main = hspec $ do
    describe "parseStringCoder" $ do
        context "when working with letters" $ do
            it "works" $
                aps "alpha" [Right "abc"] `shouldBe` Left [Right 1, Right 2, Right 3]

            it "accepts extras" $
                aps "alpha" [Right "a?b%c"] `shouldBe` Left [Right 1, Left (CxExtra "?"), Right 2, Left (CxExtra "%"), Right 3]

            it "does not consider spaces delimiters" $ do
                aps "alpha" [Right "a b c"] `shouldBe` Left [Right 1, Left (CxExtra " "), Right 2, Left (CxExtra " "), Right 3]
                aps "alpha" [Right "S   PACE"] `shouldBe` Left [Right 19, Left (CxExtra "   "), Right 16, Right 1, Right 3, Right 5]

            it "does not expand content spaces" $ do
                aps "alpha" [Right " "] `shouldBe` Left [Left (CxExtra " ")]
                aps "alpha" [Right "  "] `shouldBe` Left [Left (CxExtra "  ")]

            it "preserves known extra spaces" $ do
                aps "alpha" [Right "a", Left (CxExtra " "), Right "b"] `shouldBe` Left [Right 1, Left (CxExtra " "), Right 2]
                aps "alpha" [Right "m", Left (CxExtra "   "), Right "w"] `shouldBe` Left [Right 13, Left (CxExtra "   "), Right 23]

            it "preserves known delimiter spaces and commas" $ do
                aps "alpha" [Right "a", Left (CxDelim " "), Right "b"] `shouldBe` Left [Right 1, Left (CxDelim " "), Right 2]
                aps "alpha" [Right "h", Left (CxDelim ","), Right "s"] `shouldBe` Left [Right 8, Left (CxDelim ","), Right 19]

        context "when working with numbers" $ do
            it "works" $ do
                aps "numbers" [Right "42 13 37"] `shouldBe` Left [Right 42, Left (CxDelim " "), Right 13, Left (CxDelim " "), Right 37]
                aps "decimal" [Right "42 13 37"] `shouldBe` Left [Right 42, Left (CxDelim " "), Right 13, Left (CxDelim " "), Right 37]

            it "works with hexadecimal" $ do
                aps "hex"         [Right "10 20 30"] `shouldBe` Left [Right 16, Left (CxDelim " "), Right 32, Left (CxDelim " "), Right 48]
                aps "hexadecimal" [Right "10 20 30"] `shouldBe` Left [Right 16, Left (CxDelim " "), Right 32, Left (CxDelim " "), Right 48]
            it "works with octal" $ do
                aps "oct"   [Right "10 20 30"] `shouldBe` Left [Right 8, Left (CxDelim " "), Right 16, Left (CxDelim " "), Right 24]
                aps "octal" [Right "10 20 30"] `shouldBe` Left [Right 8, Left (CxDelim " "), Right 16, Left (CxDelim " "), Right 24]
            it "works with binary" $ do
                aps "bin"    [Right "10 100 1000"] `shouldBe` Left [Right 2, Left (CxDelim " "), Right 4, Left (CxDelim " "), Right 8]
                aps "binary" [Right "10 100 1000"] `shouldBe` Left [Right 2, Left (CxDelim " "), Right 4, Left (CxDelim " "), Right 8]
            it "works with specified bases" $ do
                aps "base 11" [Right "a1"] `shouldBe` Left [Right 111]
                aps "base 13" [Right "10 26 100"] `shouldBe` Left [Right 13, Left (CxDelim " "), Right 32, Left (CxDelim " "), Right 169]
                aps "base 36" [Right "darn"] `shouldBe` Left [Right 620483]

            it "accepts commas as delimiters" $ do
                aps "numbers" [Right "1,2"] `shouldBe` Left [Right 1, Left (CxDelim ","), Right 2]
                aps "numbers" [Right "42,13,37"] `shouldBe` Left [Right 42, Left (CxDelim ","), Right 13, Left (CxDelim ","), Right 37]
            it "does not accept double or multiple commas as delimiters" $ do
                aps "numbers" [Right "1,,,,,2"] `shouldBe` Left [Right 1, Left (CxExtra ",,,,,"), Right 2]
                aps "numbers" [Right "42,,13,,37"] `shouldBe` Left [Right 42, Left (CxExtra ",,"), Right 13, Left (CxExtra ",,"), Right 37]
            it "does not accept other garbage as delimiters" $ do
                aps "numbers" [Right "42;13.37"] `shouldBe` Left [Right 42, Left (CxExtra ";"), Right 13, Left (CxExtra "."), Right 37]
                aps "numbers" [Right "207/253=492"] `shouldBe` Left [Right 207, Left (CxExtra "/"), Right 253, Left (CxExtra "="), Right 492]
            it "shrinks spaces to be extras" $ do
                aps "numbers" [Right "42  13   37"] `shouldBe` Left [Right 42, Left (CxExtra " "), Right 13, Left (CxExtra "  "), Right 37]
                aps "numbers" [Right "  "] `shouldBe` Left [Left (CxExtra " ")]

            it "preserves delimiters" $ do
                aps "numbers" [Left (CxDelim " " )] `shouldBe` Left [Left (CxDelim " " )]
                aps "numbers" [Left (CxDelim "," )] `shouldBe` Left [Left (CxDelim "," )]
                aps "numbers" [Left (CxDelim "  ")] `shouldBe` Left [Left (CxDelim "  ")]
            it "shrinks extras" $ do
                aps "numbers" [Left (CxExtra "  ")] `shouldBe` Left [Left (CxExtra " ")]
                aps "numbers" [Left (CxExtra "   ")] `shouldBe` Left [Left (CxExtra "  ")]

        context "when working with tokens of digits" $ do
            it "works with bits" $ do
                aps "8 bits" [Right "001110100010110100101001"] `shouldBe` Left [Right 58, Right 45, Right 41]
                aps "7 bits" [Right "011101001011010101001"] `shouldBe` Left [Right 58, Right 45, Right 41]
            it "works with digits" $ do
                aps "2 digits" [Right "140927"   ] `shouldBe` Left [Right 14, Right 9, Right 27]
                aps "3 digits" [Right "014009027"] `shouldBe` Left [Right 14, Right 9, Right 27]
            it "works with nybbles" $ do
                aps "2 nybbles" [Right "3a2D29"] `shouldBe` Left [Right 58, Right 45, Right 41]
                aps "3 nybbles" [Right "03a02D029"] `shouldBe` Left [Right 58, Right 45, Right 41]
            it "works with bytes" $ do
                aps "1 byte" [Right "3a2D29"] `shouldBe` Left [Right 58, Right 45, Right 41]
                aps "2 byte" [Right "003a002D0029"] `shouldBe` Left [Right 58, Right 45, Right 41]

            it "takes single spaces as delimiters" $
                aps "8 bits" [Right "00111010 00101101 00101001"] `shouldBe` Left [Right 58, Left (CxDelim " "), Right 45, Left (CxDelim " "), Right 41]
            it "keeps double spaces" $
                aps "8 bits" [Right "00111010  00101101  00101001"] `shouldBe` Left [Right 58, Left (CxDelim "  "), Right 45, Left (CxDelim "  "), Right 41]
            it "preserves known delimiter spaces" $
                aps "9 bits" [Right "011111101", Left (CxDelim " "), Right "111101100"] `shouldBe` Left [Right 253, Left (CxDelim " "), Right 492]
            it "preserves known extra spaces" $
                aps "9 bits" [Right "011111101", Left (CxExtra " "), Right "111101100"] `shouldBe` Left [Right 253, Left (CxExtra " "), Right 492]

        context "when working with base64" $ do
            it "works" $
                aps "base64" [Right "TWFu"] `shouldBe` Left [Right 77, Right 97, Right 110]
            it "handles one equals" $
                aps "base64" [Right "ZS4="] `shouldBe` Left [Right 101, Right 46]
            it "handles two equals" $
                aps "base64" [Right "Lg=="] `shouldBe` Left [Right 46]
            it "considers spaces extras" $ do
                aps "base64" [Right "QQ== QQ==" ] `shouldBe` Left [Right 65, Left (CxExtra " " ), Right 65]
                aps "base64" [Right "QQ==  QQ=="] `shouldBe` Left [Right 65, Left (CxExtra "  "), Right 65]
            it "considers garbage extras" $
                aps "base64" [Right "QQ==@**@QQ=="] `shouldBe` Left [Right 65, Left (CxExtra "@**@"), Right 65]
            it "preserves known delimiter spaces" $
                aps "base64" [Right "QQ==", Left (CxDelim " "), Right "QQ=="] `shouldBe` Left [Right 65, Left (CxDelim " "), Right 65]
            it "preserves known extra spaces" $
                aps "base64" [Right "QQ==", Left (CxExtra " "), Right "QQ=="] `shouldBe` Left [Right 65, Left (CxExtra " "), Right 65]

        context "when working with morse" $ do
            it "can convert from morse" $
                aps "morse" [Right "--- .-. --.."] `shouldBe` Right [Right "orz"]
            it "can convert from weird morse" $
                aps "morse" [Right "---.. ...- -.--.-"] `shouldBe` Right [Right "8v)"]
            it "can convert to morse" $
                aps "to morse" [Right "orz"] `shouldBe` Right [Right "---", Left (CxDelim " "), Right ".-.", Left (CxDelim " "), Right "--.."]
            it "can convert to weird morse" $
                aps "to morse" [Right "8v)"] `shouldBe` Right [Right "---..", Left (CxDelim " "), Right "...-", Left (CxDelim " "), Right "-.--.-"]
            it "produces ' / ' extras" $
                aps "to morse" [Right "a ok"] `shouldBe` Right [Right ".-", Left (CxExtra " / "), Right "---", Left (CxDelim " "), Right "-.-"]
            it "produces ' / ' extras from delimiters" $
                aps "to morse" [Right "g", Left (CxDelim " "), Right "g"] `shouldBe` Right [Right "--.", Left (CxExtra " / "), Right "--."]

        context "when working with shifts" $ do
            it "works" $
                aps "shift 1" [Right "abcxyz"] `shouldBe` Right [Right "bcdyza"]
            it "lets weird characters pass through" $
                aps "shift 13" [Right "< cat.PNG~ >"] `shouldBe` Right [Right "< png.CAT~ >"]
            it "shifts by i, skipping punctuation" $
                aps "shift i" [Right "aaa; aaa?"] `shouldBe` Right [Right "abc; def?"]
            it "shifts by 2i" $
                aps "shift 2i" [Right "aaa; aaa!"] `shouldBe` Right [Right "ace; gik!"]
            it "shifts by 3i+1" $
                aps "shift 3i+1" [Right "aaa; aaa!"] `shouldBe` Right [Right "beh; knq!"]
            it "counts punctuation with exclamation mark" $
                aps "shift! i" [Right "aaa; aaa?"] `shouldBe` Right [Right "abc; fgh?"]

        context "when working with filters" $ do
            it "can drop spaces" $
                aps "drop spaces" [Right " a: b ", Left (CxBadString " c: d "), Left (CxExtra " e: f "), Left (CxDelim " g: h ")] `shouldBe` Right [Right "a:b", Left (CxBadString "c:d"), Left (CxExtra "e:f"), Left (CxDelim "g:h")]
            it "can filter letters" $
                aps "filter letters" [Right " a: b ", Left (CxBadString " c: d "), Left (CxExtra " e: f "), Left (CxDelim " g: h ")] `shouldBe` Right [Right "ab", Left (CxBadString "cd"), Left (CxExtra "ef"), Left (CxDelim "gh")]
            it "can filter digits" $
                aps "filter digits" [Right " 1/2b ", Left (CxBadString " 3^^7 "), Left (CxExtra " 0=0x "), Left (CxDelim " 4x6! ")] `shouldBe` Right [Right "12", Left (CxBadString "37"), Left (CxExtra "00"), Left (CxDelim "46")]

        context "when transforming text" $ do
            it "can translate" $
                aps "translate 123 to xyz" [Right "12332144"] `shouldBe` Right [Right "xyzzyx44"]
            it "can uppercase" $
                aps "uppercase" [Right "This is SPARTA"] `shouldBe` Right [Right "THIS IS SPARTA"]
            it "can lowercase" $
                aps "lowercase" [Right "The WORLD is quiet here"] `shouldBe` Right [Right "the world is quiet here"]

    describe "parseIntCoder" $ do
        context "when working with letters" $ do
            it "works" $ api "to alpha" [Right 15, Right 18, Right 26] `shouldBe` Right [Right "orz"]

            it "eliminates small delimiters" $ api "to alpha" [Right 3, Left (CxDelim " "), Right 24] `shouldBe` Right [Right "cx"]
            it "shrinks spaces" $ do
                api "to alpha" [Left (CxDelim "  ")] `shouldBe` Right [Left (CxExtra " ")]
                api "to alpha" [Right 1, Left (CxDelim "  "), Right 2] `shouldBe` Right [Right "a", Left (CxExtra " "), Right "b"]
            it "preserves extra spaces" $ do
                api "to alpha" [Left (CxExtra "  ")] `shouldBe` Right [Left (CxExtra "  ")]
                api "to alpha" [Left (CxExtra "   ")] `shouldBe` Right [Left (CxExtra "   ")]

        context "when working with chars" $ do
            it "works" $ api "to chars" [Right 0x38, Right 0x76, Right 0x29] `shouldBe` Right [Right "8v)"]

            it "eliminates small delimiters" $ api "to chars" [Right 0x3a, Left (CxDelim " "), Right 0x44] `shouldBe` Right [Right ":D"]
            it "shrinks spaces" $ do
                api "to chars" [Left (CxDelim "  ")] `shouldBe` Right [Left (CxExtra " ")]
                api "to chars" [Right 0x3e, Left (CxDelim "  "), Right 0x3c] `shouldBe` Right [Right ">", Left (CxExtra " "), Right "<"]
            it "preserves extra spaces" $ do
                api "to chars" [Left (CxExtra "  ")] `shouldBe` Right [Left (CxExtra "  ")]
                api "to chars" [Left (CxExtra "   ")] `shouldBe` Right [Left (CxExtra "   ")]

        context "when working with numbers" $ do
            it "works" $
                api "to numbers" [Right 253] `shouldBe` Right [Right "253"]

            it "works with binary" $ do
                api "to binary" [Right 253] `shouldBe` Right [Right "11111101"]
                api "to bin" [Right 492] `shouldBe` Right [Right "111101100"]
            it "works with hex" $ do
                api "to hexadecimal" [Right 105] `shouldBe` Right [Right "69"]
                api "to hex" [Right 255] `shouldBe` Right [Right "ff"]
            it "works with base 36" $
                api "to base 36" [Right 620483] `shouldBe` Right [Right "darn"]

            it "adds delim spaces" $
                api "to numbers" [Right 253, Right 492] `shouldBe` Right [Right "253", Left (CxDelim " "), Right "492"]
            it "preserves delim spaces" $
                api "to numbers" [Right 253, Left (CxDelim " "), Right 492] `shouldBe` Right [Right "253", Left (CxDelim " "), Right "492"]
            it "preserves delim commas" $
                api "to numbers" [Right 253, Left (CxDelim ","), Right 492] `shouldBe` Right [Right "253", Left (CxDelim ","), Right "492"]

            it "expands extra spaces" $
                api "to numbers" [Right 253, Left (CxExtra " "), Right 492] `shouldBe` Right [Right "253", Left (CxExtra "  "), Right "492"]

        context "when working with streams of digits" $ do
            it "works with bits" $
                api "to bits" [Right 1, Right 0] `shouldBe` Right [Right "10"]
            it "works with nybbles" $
                api "to nybbles" [Right 12, Right 0, Right 13, Right 14] `shouldBe` Right [Right "c0de"]
            it "works with Nybbles" $
                api "to Nybbles" [Right 12, Right 0, Right 13, Right 14] `shouldBe` Right [Right "C0DE"]

            it "eliminates small delimiter spaces" $
                api "to nybbles" [Right 7, Left (CxDelim " "), Right 15] `shouldBe` Right [Right "7f"]
            it "eliminates small delimiter commas" $
                api "to nybbles" [Right 7, Left (CxDelim ","), Right 15] `shouldBe` Right [Right "7f"]

        context "when working with tokens of digits" $ do
            it "works with bits" $
                api "to 8 bits" [Right 1, Left (CxDelim " "), Right 2] `shouldBe` Right [Right "00000001", Left (CxDelim " "), Right "00000010"]
            it "works with bytes" $
                api "to bytes" [Right 58, Left (CxDelim " "), Right 45, Left (CxDelim " "), Right 41] `shouldBe` Right [Right "3a", Left (CxDelim " "), Right "2d", Left (CxDelim " "), Right "29"]

            it "adds delim spaces" $
                api "to 8 bits" [Right 1, Right 2] `shouldBe` Right [Right "00000001", Left (CxDelim " "), Right "00000010"]

        context "when working with arithmetic" $ do
            it "can add (plus, add, +)" $ do
                api "plus 42" [Right 1, Right 2] `shouldBe` Left [Right 43, Right 44]
                api "+ 42"    [Right 1, Right 2] `shouldBe` Left [Right 43, Right 44]
            it "can subtract (minus, subtract, -)" $ do
                api "minus 100"    [Right 101, Right 102] `shouldBe` Left [Right 1, Right 2]
                api "subtract 100" [Right 101, Right 102] `shouldBe` Left [Right 1, Right 2]
                api "- 100"        [Right 101, Right 102] `shouldBe` Left [Right 1, Right 2]
            it "can multiply (times, multiply, *, x)" $ do
                api "times 37"    [Right 17, Right 18] `shouldBe` Left [Right 629, Right 666]
                api "multiply 37" [Right 17, Right 18] `shouldBe` Left [Right 629, Right 666]
                api "* 37"        [Right 17, Right 18] `shouldBe` Left [Right 629, Right 666]
                api "x 37"        [Right 17, Right 18] `shouldBe` Left [Right 629, Right 666]
            it "can mod" $
                api "mod 9" [Right 207, Right 253, Right 492] `shouldBe` Left [Right 0, Right 1, Right 6]
            it "can mod1" $
                api "mod1 9" [Right 207, Right 253, Right 492] `shouldBe` Left [Right 9, Right 1, Right 6]
            it "can negate" $ do
                api "negate"  [Right 207, Right 253, Right 492] `shouldBe` Left [Right (-207), Right (-253), Right (-492)]
                api "negated" [Right 207, Right 253, Right 492] `shouldBe` Left [Right (-207), Right (-253), Right (-492)]

            it "preserves delim spaces" $
                api "plus 1" [Right 3, Left (CxDelim " "), Right 5] `shouldBe` Left [Right 4, Left (CxDelim " "), Right 6]
            it "preserves extra spaces" $
                api "plus 1" [Right 3, Left (CxExtra " "), Right 5] `shouldBe` Left [Right 4, Left (CxExtra " "), Right 6]
            it "preserves extra garbage" $
                api "mod 10" [Right 1, Left (CxExtra "@"), Right 6677] `shouldBe` Left [Right 1, Left (CxExtra "@"), Right 7]

    modifyMaxSuccess (*2) $ describe "codex" $ do

        context "when working with letters" $ do

            context "when converting to numbers" $ do
                it "can convert letters to numbers" $
                    codexw "alpha to numbers" "abcdefghij" `shouldBe` "1 2 3 4 5 6 7 8 9 10"
                it "doubles original spaces when converting letters to numbers" $
                    codexw "alpha to numbers" "quick brown fox" `shouldBe` "17 21 9 3 11  2 18 15 23 14  6 15 24"
                it "keeps garbage when converting letters to numbers" $ do
                    codexw "alpha to numbers" "quick / brown / fox" `shouldBe` "17 21 9 3 11 / 2 18 15 23 14 / 6 15 24"
                    codexw "alpha to numbers" "(foo bar) .@_@. (baz quux)" `shouldBe` "(6 15 15  2 1 18) .@_@. (2 1 26  17 21 21 24)"
                it "can convert letters to bytes" $
                    codexw "alpha to bytes" "(foo bar) (baz quux)" `shouldBe` "(06 0f 0f  02 01 12) (02 01 1a  11 15 15 18)"

                it "is invertible" $
                    forAll (listOf (choose ('a','z'))) (\s -> codexw "alpha to numbers numbers to alpha" s === s)
                it "expands spaces" $ do
                    codexw "alpha to numbers" "ab cd" `shouldBe` "1 2  3 4"
                    codexw "alpha to numbers" " " `shouldBe` "  "
                    codexw "alpha to numbers" "       " `shouldBe` "        "
                it "expands double spaces" $ do
                    codexw "alpha to numbers" "ab cd  ef   gh" `shouldBe` "1 2  3 4   5 6    7 8"
                    codexw "alpha to numbers" "sp        ACE" `shouldBe` "19 16         1 3 5"
                it "preserves spaces on roundtrip" $ do
                    codexw "alpha to numbers numbers to alpha" "a b" `shouldBe` "a b"
                    codexw "alpha to numbers numbers to alpha" "a  b" `shouldBe` "a  b"
                it "is invertible despite garbage" $
                    forAll (listOf (arbitrary `suchThat` (\c -> c < 'A' || c > 'Z'))) (\s -> codexw "alpha to numbers numbers to alpha" s === s)

            context "when converting from numbers" $ do

                it "can convert numbers to letters"   $ codexw "numbers to alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "abcdefghij"
                it "can convert numbers to UPPERCASE" $ codexw "numbers to Alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "ABCDEFGHIJ"

                it "works with hexadecimal" $ codexw "hex to number" "ff" `shouldBe` "255"
                it "works with base 36" $ codexw "base 36 to number" "1a" `shouldBe` "46"

                it "drops commas" $ codexw "numbers to alpha" "1,2,3,4,5" `shouldBe` "abcde"
                it "does not drop other punctuation" $ codexw "numbers to alpha" "1;2;3;4;5" `shouldBe` "a;b;c;d;e"
                it "makes doubled spaces single" $
                    codexw "numbers to alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "(foo bar) (baz quux)"
                it "can strip nonletters" $
                    codexw "numbers to alpha only alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "foobarbazquux"
                it "condenses spaces" $ do
                    codexw "numbers to alpha" "1 2  3 4" `shouldBe` "ab cd"
                    codexw "numbers to alpha" "  " `shouldBe` " "
                    codexw "numbers to alpha" "        " `shouldBe` "       "
                it "preserves spaces on roundtrip" $ do
                    codexw "numbers to alpha alpha to numbers" "1 2" `shouldBe` "1 2"
                    codexw "numbers to alpha alpha to numbers" "1  2" `shouldBe` "1  2"
                it "is invertible" $
                    forAll (listOf (choose (1 :: Int,26))) (\ns ->
                        let s = unwords (map show ns) in codexw "numbers to alpha alpha to numbers" s === s)

        context "when converting between bases" $ do

            it "can convert numbers to binary" $
                codexw "numbers to binary" "0 1 2 3 4 5 6 7" `shouldBe` "0 1 10 11 100 101 110 111"
            it "can convert binary to numbers" $
                codexw "binary to numbers" "0 1 10 11 100 101 110 111" `shouldBe` "0 1 2 3 4 5 6 7"

            it "keeps junk" $
                codexw "numbers to binary" "0,1.2/3;4-5=6_7" `shouldBe` "0,1.10/11;100-101=110_111"
            it "is invertible" $
                forAll (listOf (arbitrary :: Gen (NonNegative Int))) (\ns ->
                    let s = unwords (map (show . getNonNegative) ns) in codexw "numbers to binary binary to numbers" s === s)
            it "keeps double spaces" $ do
                codexw "numbers to binary" "0  1  2  3" `shouldBe` "0  1  10  11"
                codexw "binary to numbers" "0  1  10  11" `shouldBe` "0  1  2  3"
            it "is mostly invertible despite garbage" $
                forAll arbitrary (\s -> codexw "numbers to binary binary to numbers" s === codexw "numbers to numbers" s)

        context "when working with characters" $ do

            context "when converting from characters" $ do
                it "can convert chars to bytes" $ codexw "chars to bytes" ":-)" `shouldBe` "3a 2d 29"
                it "can convert chars to Bytes" $ codexw "chars to Bytes" ":-)" `shouldBe` "3A 2D 29"
                it "can strip spaces" $ codexw "chars to Bytes strip spaces" ":-)" `shouldBe` "3A2D29"
                it "can convert chars to numbers" $ codexw "chars to numbers" ":-)" `shouldBe` "58 45 41"

            context "when converting to characters" $ do
                it "can convert bytes to chars" $ codexw "bytes to chars" "3a 2D 29" `shouldBe` ":-)"
                it "works with spaces" $ codexw "bytes to chars" "3a 2D 29  3A 2d 28" `shouldBe` ":-) :-("
                it "shrinks spaces" $ codexw "bytes to chars" "2a   2a    2a 2a     2a" `shouldBe` "*  *   **    *"
                it "works with garbage" $ codexw "bytes to chars" "[3a 2D 29]" `shouldBe` "[:-)]"
                it "can convert numbers to chars" $ codexw "numbers to chars" "58 45 41" `shouldBe` ":-)"
                it "can convert numbers to chars with spaces" $ codexw "numbers to chars" "58 45 41  58 45 40" `shouldBe` ":-) :-("

        context "when working with streams of bits" $ do

            it "can convert to numbers" $ codexw "8 bits to numbers" "00111010 00101101 00101001" `shouldBe` "58 45 41"
            it "can convert to chars" $ codexw "8 bits to chars" "00111010 00101101 00101001" `shouldBe` ":-)"

            context "when converting 8 bits to bytes" $ do
                it "works" $ codexw "8 bits to bytes" "00111010 00101101 00101001" `shouldBe` "3a 2d 29"
                it "works with continuous bits" $ codexw "8 bits to bytes" "001110100010110100101001" `shouldBe` "3a 2d 29"
                it "can strip spaces" $ codexw "8 bits to bytes strip spaces" "001110100010110100101001" `shouldBe` "3a2d29"
                it "keeps extra spaces" $ codexw "8 bits to bytes" "00111010  00101101  00101001" `shouldBe` "3a  2d  29"

        context "when working with base64" $ do
            context "when converting to base64" $ do
                it "works" $ codexw "chars to base64" "any carnal pleasure" `shouldBe` "YW55IGNhcm5hbCBwbGVhc3VyZQ=="
                it "handles weird characters" $ codexw "chars to base64" "?>?>?>" `shouldBe` "Pz4/Pj8+"
            context "when converting from base64" $ do
                it "works" $ codexw "base64 to chars" "YW55IGNhcm5hbCBwbGVhc3VyZQ==" `shouldBe` "any carnal pleasure"
                it "iterates" $ codexw "base64 to chars base64 to chars base64 to chars" "VUVRNEwxQm5QVDA9" `shouldBe` "<??>"
                it "handles weird characters" $ codexw "base64 to chars" "Pz4/Pj8+" `shouldBe` "?>?>?>"
            it "is invertible" $
                forAll (listOf (choose (0 :: Int,255))) (\ns ->
                    let s = unwords (map show ns) in codexw "numbers to base64 base64 to numbers" s === s)

        context "when performing Caesar shifts and company" $ do
            it "works" $ do
                codexw "shift 3" "primero" `shouldBe` "sulphur"
                codexw "shift 23" "aBcdEf" `shouldBe` "xYzaBc"
            it "works with rot13 shortcut" $ codexw "rot13" "abjurer NoWhErE" `shouldBe` "nowhere AbJuReR"
            it "works with rot47" $ do
                codexw "rot47" "The Quick Brown Fox Jumps Over The Lazy Dog." `shouldBe` "%96 \"F:4< qC@H? u@I yF>AD ~G6C %96 {2KJ s@8]"
                codexw "rot47" "%96 \"F:4< qC@H? u@I yF>AD ~G6C %96 {2KJ s@8]" `shouldBe` "The Quick Brown Fox Jumps Over The Lazy Dog."
            it "supports atbash" $ codexw "atbash" "holy ark SLOB ZiP" `shouldBe` "slob zip HOLY ArK"
            it "shifting by 3 and 23 are inverses" $ forAll (listOf arbitrary) (\s -> codexw "shift 3" (codexw "shift 23" s) === s)
            it "shifting by 7 and 19 are inverses" $ forAll (listOf arbitrary) (\s -> codexw "shift 7" (codexw "shift 19" s) === s)
            it "rot13 is self-inverse" $
                forAll (listOf arbitrary) (\s -> codexw "rot13" (codexw "rot13" s) === s)
            it "rot47 is self-inverse" $
                forAll (listOf arbitrary) (\s -> codexw "rot47" (codexw "rot47" s) === s)
            it "atbash is self-inverse" $
                forAll (listOf arbitrary) (\s -> codexw "atbash" (codexw "atbash" s) === s)

        context "when working with morse code" $ do
            it "can convert from morse code" $
                codexw "morse" "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
                    `shouldBe` "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
            it "can convert to morse code" $
                codexw "to morse" "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
                    `shouldBe` "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
            it "is invertible" $
                forAll
                    (listOf (oneof [choose ('a','z'), choose ('0','9'), elements ",.?;:'-/()_"]))
                    (\s -> codexw "to morse morse" s === s)

        context "when translating" $ do
            it "can translate" $
                codexw "translate xy to 01" "xyyx :) yxxy" `shouldBe` "0110 :) 1001"
            it "can translate by lengthening second argument" $
                codexw "translate xyz to 01" "foo bar xyzzy" `shouldBe` "foo bar 01111"

        context "when converting between cases" $ do
            it "can convert to uppercase" $
                codexw "uppercase" "foo BAR baz QUUX" `shouldBe` "FOO BAR BAZ QUUX"
            it "can convert to lowercase" $
                codexw "lowercase" "foo BAR baz QUUX" `shouldBe` "foo bar baz quux"

        context "when performing char maps" $ do
            it "circled is invertible" $
                forAll (listOf (choose ('a','z'))) (\s -> codexw "circled uncircled" s === s)
            it "fullwidth is invertible" $
                forAll (listOf (choose ('a','z'))) (\s -> codexw "fullwidth halfwidth" s === s)
            it "smallcaps is invertible" $
                forAll (listOf (choose ('a','z'))) (\s -> codexw "smallcaps unsmallcaps" s === s)
            it "smallcaps+ is invertible" $
                forAll (listOf (choose ('a','z'))) (\s -> codexw "smallcaps+ unsmallcaps" s === s)

        context "when filtering" $ do
            it "can filter letters" $ codexw "filter letters" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUXquickbrownfox"
            it "can strip spaces" $ codexw "strip spaces" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUX/quickbrownfox"
            it "can filter vowels" $ codexw "filter vowels" "facetiously ;_; FACETIOUSLY" `shouldBe` "aeiouAEIOU"
            it "can strip consonants" $ codexw "strip consonants" "Functional programming in Haskell" `shouldBe` "uioa oai i ae"
            it "only has letters after filtering" $
                forAll arbitrary (all isLetter . codexw "filter letters")
            it "has no spaces after stripping" $
                forAll arbitrary (not . any isSpace . codexw "strip whitespace")

        context "when round tripping to morse" $ do
            it "handles stage 1" $ codexw "alpha to numbers" "complex" `shouldBe` "3 15 13 16 12 5 24"
            it "handles stage 2" $ codexw "alpha to numbers to morse" "complex" `shouldBe` "...-- / .---- ..... / .---- ...-- / .---- -.... / .---- ..--- / ..... / ..--- ....-"
            it "handles stage 3" $ codexw "alpha to numbers to morse morse" "complex" `shouldBe` "3 15 13 16 12 5 24"
            it "handles stage 4" $ codexw "alpha to numbers to morse morse numbers to alpha" "complex" `shouldBe` "complex"

        context "when round tripping to chars" $ do
            it "handles stage 1" $ codexw "chars to bytes" "6*9=42" `shouldBe` "36 2a 39 3d 34 32"
            it "handles stage 2" $ codexw "chars to bytes chars to bytes" "6*9=42" `shouldBe` "33 36 32 61 33 39 33 64 33 34 33 32"
            it "handles stage 3" $ codexw "chars to bytes chars to bytes bytes to chars" "6*9=42" `shouldBe` "362a393d3432"
            it "handles stage 4" $ codexw "chars to bytes chars to bytes bytes to chars bytes to chars" "6*9=42" `shouldBe` "6*9=42"

    where aps :: String -> CxList String -> Either (CxList Int) (CxList String)
          aps = (right groupRights .) . (. ungroupRights) . applyCxCoder . either error id . parseCharCoder . words
          api :: String -> CxList Int -> Either (CxList Int) (CxList String)
          api = (right groupRights .) . applyCxCoder . either error id . parseIntCoder . words
          codexw :: String -> String -> String
          codexw = either error id . codex . words

