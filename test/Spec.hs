{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import CSV
import CSVSpec


main :: IO ()
main = hspec $ do
    testCSVSpec
    testCSV


testCSVSpec = do
    describe "typeText" $ do
        it "parses Integers correctly" $ do
            typeText "3" `shouldBe` CSVInteger
            typeText "01" `shouldBe` CSVInteger
        it "parses everything else as Strings" $ do
            typeText "A3" `shouldBe` CSVString
            typeText "foo" `shouldBe` CSVString
    describe "findMismatch" $ do
        it "should be able to skip ok indices" $ do
            let spec = CSVSpec [CSVInteger, CSVInteger, CSVInteger]
                mismatch i = MismatchedColumn i CSVInteger CSVString
            findMismatch spec (RawRow ["A", "3", "A"])
                `shouldBe` Mismatch [mismatch 1, mismatch 3]


testCSV = do
    describe "splitRow" $ do
        it "splits simple rows correctly" $ do
            splitRow "1,2,3" `shouldBe` RawRow ["1", "2", "3"]
            splitRow " 1 , 2 , 3 " `shouldBe` RawRow ["1", "2", "3"]
        it "can handle one column lines" $ do
            splitRow "1" `shouldBe` RawRow ["1"]