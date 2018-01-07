import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Char

main :: IO ()
main = hspec $ do
    describe "List Comprehensions" $ do
        it "squares numbers from 1-5" $ do
            [x^2 | x <- [1..5]]`shouldBe` [1,4,9,16,25]
            {- The solution is: [x^2 | x <- [1..5]]  -}
        it "squares numbers from 1-5 and filters for even" $ do
            [x^2 | x <- [1..5], mod x 2 == 0] `shouldBe` [4,16]
        it "can raise the numbers from 1-5 to the power of 2 and 3" $ do
            [x^p  | x <- [1..5], p <- [2,3]] `shouldBe` [1,1,4,8,9,27,16,64,25,125]
        it "can filter the previous list to numbers below 50" $ do
            [x^p | x <- [1..5], p <- [2,3], x^p < 50] `shouldBe` [1,1,4,8,9,27,16,25]
        it "can create tuples from lists" $ do
            [(x, y) | x <- [1,2,3], y <- "ab"] `shouldBe` [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
        it "can filter for upper-case letters from \"Apple Brick Cat\"" $ do
            [x | x <- "Alpha Bravo Charlie", isUpper x] `shouldBe` "ABC"
        it "can be generalized into a function" $ do
            let f xs = [x | x <- xs, isUpper x]
            f "Apple Brick Cat" `shouldBe` "ABC"
