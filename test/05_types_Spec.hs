import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Char

{- Create the `removeNonUppercase` function with proper type -}
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase = filter isUpper

{- Create the addThree function with proper type info -}
addThree :: Num a => a -> a -> a -> a
addThree a b c = a + b + c

main :: IO ()
main = hspec $ do
    describe "Functions have types" $ do
        it "can use a function with type" $ do
            removeNonUppercase "HelloWORLD" `shouldBe` "HWORLD"
            addThree 1 2 3 `shouldBe` 6
    describe "Type classes" $ do
        it "can order strings" $ do
            "Abrakadabra" <= "Zebra" `shouldBe` True
            {- use the words "Abrakadabra"  "Zebra" -}
            compare "Abrakadabra" "Zebra" `shouldBe` LT
            "Abrakadabra" <= "Zebra" `shouldBe` True
            compare 5 3 `shouldBe` GT
        it "can show anything" $ do
            show 3 `shouldBe` "3"
            show True `shouldBe` "True"
        it "can read strings into values" $ do
            (read "True") || False `shouldBe` True
            (read "8.2") + 3.8 `shouldBe` 12
            ((read "[1,2,3,4]") :: [Int]) `shouldBe` [1,2,3,4]
            ((read "(3, 'a')") :: (Int, Char)) `shouldBe` (3, 'a')
        it "can provide ranges, next items for Enum types" $ do
            ['a'..'e'] `shouldBe` "abcde"
            [LT .. GT]`shouldBe` [LT,EQ,GT]
            [3..5] `shouldBe` [3,4,5]
            {- succ to get the next -}
            succ 'B' `shouldBe` 'C'
    describe "Num is a numeric typeclass" $ do
        it "can act like numbers" $ do
            {- use the type -}
            (20 :: Int) `shouldBe` 20
    describe "fromIntegral is there historical reasons" $ do
        it "can add Int and Floating point numbers" $ do
            {- from... function -}
            fromIntegral (length [1,2,3,4]) + 3.2 `shouldBe` 7.2
