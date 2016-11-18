module LibSpec where

import Test.Hspec
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "wrapBrackets" $ do
    it "Wraps a string in brackets" $ do
      wrapBrackets "x" `shouldBe` "[x]"
  
  describe "showUnreadableChar" $ do
    it "Shows null chars as '_', all else as '*'" $ do
      showUnreadableChr '\0' `shouldBe` '_'

  describe "_overwriteIf" $ do
    it "It merges two lists, based on comparison of items, pads shorter list." $ do
      _overwriteIf _nib '\0' "abcd" "z" `shouldBe` "zbcd"
      _overwriteIf _nib '\0' "abcd" "w\0yz" `shouldBe` "wbyz"

  describe "mergeLines" $ do
    it "Merges lines of text, be default, the newest line overwrites, except nulls" $ do
      mergeLines ["abcd", "efgh"] `shouldBe` "efgh"
      mergeLines ["abcd", "wx"] `shouldBe` "wxcd"
      mergeLines ["abcd", "w\0yz"] `shouldBe` "wbyz"

  describe "indexToDelta" $ do
    it "Takes a list of list indexes, and converts it to a list of segment lengths" $ do
      indexToDelta [2,4,8] `shouldBe` [2,2,4]
      indexToDelta [5,6,20] `shouldBe` [5,1,14]
