-- file Spec.hs
import Test.Hspec
import Text.Bcodex

main :: IO ()
main = hspec $ do
    describe "codex" $ do
        context "when working with letters" $ do
            context "when converting to numbers" $ do
                it "can convert letters to numbers" $ do
                    codexw "alpha to numbers" "abcdefghij" `shouldBe` "1 2 3 4 5 6 7 8 9 10"
                it "doubles original spaces when converting letters to numbers" $ do
                    codexw "alpha to numbers" "quick brown fox" `shouldBe` "17 21 9 3 11  2 18 15 23 14  6 15 24"
                it "keeps garbage when converting letters to numbers" $ do
                    codexw "alpha to numbers" "quick / brown / fox" `shouldBe` "17 21 9 3 11 / 2 18 15 23 14 / 6 15 24"
                    codexw "alpha to numbers" "(foo bar) (baz quux)" `shouldBe` "(6 15 15  2 1 18) (2 1 26  17 21 21 24)"
                it "can convert letters to bytes" $ do
                    codexw "alpha to bytes" "(foo bar) (baz quux)" `shouldBe` "(06 0f 0f  02 01 12) (02 01 1a  11 15 15 18)"
            context "when converting from numbers" $ do
                it "can convert numbers to letters" $ do codexw "numbers to alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "abcdefghij"
                it "can convert numbers to UPPERCASE" $ do codexw "numbers to Alpha" "1 2 3 4 5 6 7 8 9 10" `shouldBe` "ABCDEFGHIJ"
                it "makes doubled spaces single" $ do codexw "numbers to alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "(foo bar) (baz quux)"
                it "can strip nonletters" $ do codexw "numbers to alpha only alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" `shouldBe` "foobarbazquux"
        context "when converting between bases" $ do
            it "can convert numbers to binary" $ do codexw "numbers to binary" "1 2 3 4 5 6 7" `shouldBe` "1 10 11 100 101 110 111"
            it "can convert binary to numbers" $ do codexw "binary to numbers" "1 10 11 100 101 110 111" `shouldBe` "1 2 3 4 5 6 7"
        context "when working with characters" $ do
            context "when converting from characters" $ do
                it "can convert chars to bytes" $ do codexw "chars to bytes" ":-)" `shouldBe` "3a 2d 29"
                it "can convert chars to Bytes" $ do codexw "chars to Bytes" ":-)" `shouldBe` "3A 2D 29"
                it "can strip spaces" $ do codexw "chars to Bytes strip spaces" ":-)" `shouldBe` "3A2D29"
                it "can convert chars to numbers" $ do codexw "chars to numbers" ":-)" `shouldBe` "58 45 41"
            context "when converting to characters" $ do
                it "can convert bytes to chars" $ do codexw "bytes to chars" "3a 2D 29" `shouldBe` ":-)"
                it "works with spaces" $ do codexw "bytes to chars" "3a 2D 29  3A 2d 28" `shouldBe` ":-) :-("
                it "works with garbage" $ do codexw "bytes to chars" "[3a 2D 29]" `shouldBe` "[:-)]"
                it "can convert numbers to chars" $ do codexw "numbers to chars" "58 45 41" `shouldBe` ":-)"
                it "can convert numbers to chars with spaces" $ do codexw "numbers to chars" "58 45 41  58 45 40" `shouldBe` ":-) :-("
        context "when working with streams of bits" $ do
            context "when converting 8 bits to bytes" $ do
                it "works" $ do codexw "8 bits to bytes" "00111010 00101101 00101001" `shouldBe` "3a 2d 29"
                it "works with continuous bits" $ do codexw "8 bits to bytes" "001110100010110100101001" `shouldBe` "3a 2d 29"
                it "can strip spaces" $ do codexw "8 bits to bytes strip spaces" "001110100010110100101001" `shouldBe` "3a2d29"
                it "keeps extra spaces" $ do codexw "8 bits to bytes" "00111010  00101101  00101001" `shouldBe` "3a  2d  29"
        context "when working with base64" $ do
            context "when converting to base64" $ do
                it "works" $ do codexw "chars to base64" "any carnal pleasure" `shouldBe` "YW55IGNhcm5hbCBwbGVhc3VyZQ=="
                it "handles weird characters" $ do codexw "chars to base64" "?>?>?>" `shouldBe` "Pz4/Pj8+"
            context "when converting from base64" $ do
                it "works" $ do codexw "base64 to chars" "YW55IGNhcm5hbCBwbGVhc3VyZQ==" `shouldBe` "any carnal pleasure"
                it "handles weird characters" $ do codexw "base64 to chars" "Pz4/Pj8+" `shouldBe` "?>?>?>"
        context "when performing Caesar shifts" $ do
            it "works" $ do
                codexw "shift 3" "primero" `shouldBe` "sulphur"
                codexw "shift 23" "aBcdEf" `shouldBe` "xYzaBc"
            it "works with rot13 shortcut" $ do codexw "rot13" "abjurer NoWhErE" `shouldBe` "nowhere AbJuReR"
        context "when working with morse code" $ do
            it "can convert from morse code" $ do codexw "morse" "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----" `shouldBe` "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
            it "can convert to morse code" $ do codexw "to morse" "foo bar abcdefghijklmnopqrstuvwxyz1234567890" `shouldBe` "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
        context "when translating" $ do
            it "can translate" $ do codexw "translate xyz to 01" "foo bar xyzzy" `shouldBe` "foo bar 01111"
        context "when converting between cases" $ do
            it "can convert to uppercase" $ do codexw "uppercase" "foo BAR baz QUUX" `shouldBe` "FOO BAR BAZ QUUX"
            it "can convert to lowercase" $ do codexw "lowercase" "foo BAR baz QUUX" `shouldBe` "foo bar baz quux"
        context "when filtering" $ do
            it "can filter" $ do codexw "filter letters" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUXquickbrownfox"
            it "can strip" $ do codexw "strip spaces" "foo BAR baz QUUX / quick brown fox" `shouldBe` "fooBARbazQUUX/quickbrownfox"
    where codexw = either error id . codex . words

