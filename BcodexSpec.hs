-- file Spec.hs
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Bcodex
import Data.Char

main :: IO ()
main = hspec $ do
    describe "parseStringCoder" $ do
        context "when working with letters" $ do
            it "works" $
                aps "alpha" [Right "abc"] `shouldBe` Left [Right 1, Right 2, Right 3]

            it "accepts extras" $
                aps "alpha" [Right "a?b%c"] `shouldBe` Left [Right 1, Left (CxExtra "?"), Right 2, Left (CxExtra "%"), Right 3]

            it "does not consider spaces delimiters" $
                aps "alpha" [Right "a b c"] `shouldBe` Left [Right 1, Left (CxExtra " "), Right 2, Left (CxExtra " "), Right 3]

        context "when working with numbers" $ do
            it "works" $
                aps "numbers" [Right "42 13 37"] `shouldBe` Left [Right 42, Left (CxDelim " "), Right 13, Left (CxDelim " "), Right 37]

        context "when working with streams of bits" $ do
            it "works" $ aps "8 bits" [Right "001110100010110100101001"] `shouldBe` Left [Right 58, Right 45, Right 41]
            it "takes single spaces as delimiters" $ aps "8 bits" [Right "00111010 00101101 00101001"] `shouldBe` Left [Right 58, Left (CxDelim " "), Right 45, Left (CxDelim " "), Right 41]
            it "keeps double spaces" $ aps "8 bits" [Right "00111010  00101101  00101001"] `shouldBe` Left [Right 58, Left (CxExtra "  "), Right 45, Left (CxExtra "  "), Right 41]

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
                it "is invertible despite garbage" $
                    forAll (listOf (arbitrary `suchThat` (\c -> c < 'A' || c > 'Z'))) (\s -> codexw "alpha to numbers numbers to alpha" s === s)
            context "when converting from numbers" $ do
                it "can convert numbers to letters" $ codexw "numbers to alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "abcdefghij"
                it "can convert numbers to UPPERCASE" $ codexw "numbers to Alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "ABCDEFGHIJ"
                it "drops commas" $ codexw "numbers to alpha" "1,2,3,4,5" `shouldBe` "abcde"
                it "does not drop other punctuation" $ codexw "numbers to alpha" "1;2;3;4;5" `shouldBe` "a;b;c;d;e"
                it "makes doubled spaces single" $
                    codexw "numbers to alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "(foo bar) (baz quux)"
                it "can strip nonletters" $
                    codexw "numbers to alpha only alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "foobarbazquux"
                it "is invertible" $
                    forAll (listOf (choose (1 :: Int,26))) (\ns ->
                        let s = unwords (map show ns) in codexw "numbers to alpha alpha to numbers" s === s)
        context "when converting between bases" $ do
            it "can convert numbers to binary" $ do codexw "numbers to binary" "0 1 2 3 4 5 6 7" `shouldBe` "0 1 10 11 100 101 110 111"
            it "can convert binary to numbers" $ do codexw "binary to numbers" "0 1 10 11 100 101 110 111" `shouldBe` "0 1 2 3 4 5 6 7"
            it "keeps junk" $ do codexw "numbers to binary" "0,1.2/3;4-5=6_7" `shouldBe` "0,1.10/11;100-101=110_111"
            it "is invertible" $
                forAll (listOf (arbitrary :: Gen (NonNegative Int))) (\ns ->
                    let s = unwords (map (show . getNonNegative) ns) in codexw "numbers to binary binary to numbers" s === s)
            it "is mostly invertible despite garbage" $
                forAll arbitrary $ (\s -> codexw "numbers to binary binary to numbers" s === codexw "numbers to numbers" s)
        context "when working with characters" $ do
            context "when converting from characters" $ do
                it "can convert chars to bytes" $ codexw "chars to bytes" ":-)" `shouldBe` "3a 2d 29"
                it "can convert chars to Bytes" $ codexw "chars to Bytes" ":-)" `shouldBe` "3A 2D 29"
                it "can strip spaces" $ codexw "chars to Bytes strip spaces" ":-)" `shouldBe` "3A2D29"
                it "can convert chars to numbers" $ codexw "chars to numbers" ":-)" `shouldBe` "58 45 41"
            context "when converting to characters" $ do
                it "can convert bytes to chars" $ codexw "bytes to chars" "3a 2D 29" `shouldBe` ":-)"
                it "works with spaces" $ codexw "bytes to chars" "3a 2D 29  3A 2d 28" `shouldBe` ":-) :-("
                it "works with garbage" $ codexw "bytes to chars" "[3a 2D 29]" `shouldBe` "[:-)]"
                it "can convert numbers to chars" $ codexw "numbers to chars" "58 45 41" `shouldBe` ":-)"
                it "can convert numbers to chars with spaces" $ codexw "numbers to chars" "58 45 41  58 45 40" `shouldBe` ":-) :-("
        context "when working with streams of bits" $ do
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
                it "handles weird characters" $ codexw "base64 to chars" "Pz4/Pj8+" `shouldBe` "?>?>?>"
        context "when performing Caesar shifts" $ do
            it "works" $ do
                codexw "shift 3" "primero" `shouldBe` "sulphur"
                codexw "shift 23" "aBcdEf" `shouldBe` "xYzaBc"
            it "works with rot13 shortcut" $ codexw "rot13" "abjurer NoWhErE" `shouldBe` "nowhere AbJuReR"
            it "shifting by 3 and 23 are inverses" $ forAll (listOf arbitrary) (\s -> codexw "shift 3" (codexw "shift 23" s) === s)
            it "shifting by 7 and 19 are inverses" $ forAll (listOf arbitrary) (\s -> codexw "shift 7" (codexw "shift 19" s) === s)
            it "rot13 is self-inverse" $
                forAll (listOf arbitrary) (\s -> codexw "rot13" (codexw "rot13" s) === s)
        context "when working with morse code" $ do
            it "can convert from morse code" $ do
                codexw "morse" "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
                    `shouldBe` "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
            it "can convert to morse code" $ do
                codexw "to morse" "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
                    `shouldBe` "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
            it "is invertible" $
                forAll
                    (listOf (oneof [choose ('a','z'), choose ('0','9'), elements ",.?;:'-/()_"]))
                    (\s -> codexw "to morse morse" s === s)
        context "when translating" $ do
            it "can translate" $ do codexw "translate xyz to 01" "foo bar xyzzy" `shouldBe` "foo bar 01111"
        context "when converting between cases" $ do
            it "can convert to uppercase" $ do codexw "uppercase" "foo BAR baz QUUX" `shouldBe` "FOO BAR BAZ QUUX"
            it "can convert to lowercase" $ do codexw "lowercase" "foo BAR baz QUUX" `shouldBe` "foo bar baz quux"
        context "when filtering" $ do
            it "can filter letters" $ do codexw "filter letters" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUXquickbrownfox"
            it "can strip spaces" $ do codexw "strip spaces" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUX/quickbrownfox"
            it "only has letters after filtering" $
                forAll arbitrary (\s -> all isLetter $ codexw "filter letters" s)
            it "has no spaces after stripping" $
                forAll arbitrary (\s -> not . any isSpace $ codexw "strip spaces" s)
    where aps = applyCxCoder . either error id . parseStringCoder . words
          codexw = either error id . codex . words

