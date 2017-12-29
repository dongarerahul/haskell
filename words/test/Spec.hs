import Test.Hspec
import Lib
import Data (grid, languages)

main :: IO ()
main = hspec $ do
  describe "Format Grid" $ do
    it "Should be able to concatenate everyline with a newline" $ do
      (formatGrid ["abc", "edf", "hij"]) `shouldBe` "abc\nedf\nhij\n"

  describe "Find Word" $ do
    it "Should be able to find word that exists in the grid" $ do
      (findWord grid "HASKELL") `shouldBe` Just "HASKELL"
      (findWord grid "PERL") `shouldBe` Just "PERL"

    it "Should NOT be able to find word that does not exist in the grid" $ do
      (findWord grid "HAMSTER") `shouldBe` Nothing

  describe "Find Words" $ do
    it "Should be able to find words that exist in the grid" $ do
      (findWords grid languages) `shouldBe` languages

    it "Should NOT be able to find words that exist in the grid" $ do
      (findWords grid ["GERMAN", "ENGLISH", "FRENCH"]) `shouldBe` []
