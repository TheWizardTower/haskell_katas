import qualified Control.Exception (PatternMatchFail, evaluate)
import           Test.Hspec
import           Test.QuickCheck

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

--charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Benjamin"
charName 'c' = "Cassidy"

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a + c, b + d)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b,  c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "Boink! This should have been a different type."
head' (x:xs) = x

tell :: [Int] -> String
tell []        = "This list is empty"
tell [x]       = "This list has one element: " ++ show x
tell [x,y]     = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (_:_:_:_) = "This list is too long"

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: Num a =>  [a] -> a
sum' [] = 0
sum' (a:xs) = a + sum' xs

firstLetter :: String -> String
firstLetter [] = "Empty string, whoops!"
firstLetter (x:xs) = "The first letter of " ++ (x:xs) ++ " is " ++ [x]

main :: IO()
main = hspec $ do
    describe "Pattern matching" $ do
        it "can be used in factorial calc" $ do
            factorial 5 `shouldBe` 120
        it "can fail when no default case" $ do
            charName 'a' `shouldBe` "Albert"
            -- charName 'd' `shouldThrow` PatternMatchFail
        it "can be used on tuples" $ do
            addVectors (1,2)(3,4) `shouldBe` (4,6)
        it "can be used on triples" $ do
            first (1,2,3) `shouldBe` 1
            second (1,2,3) `shouldBe` 2
            third (1,2,3) `shouldBe` 3
        it "can pattern list comprehensions" $ do
            let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
            [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4]
        it "can be used for the head function" $ do
            head' [2,3,4] `shouldBe` 2
            head' "Hello" `shouldBe` 'H'
        it "can safely process a list" $ do
            tell [] `shouldBe` "This list is empty"
            tell [1] `shouldBe` "This list has one element: 1"
            tell [1,2] `shouldBe` "This list has two elements: 1 and 2"
            tell [1,2,3] `shouldBe` "This list is too long"
        it "can count elements in list with recursion" $ do
            length' [] `shouldBe` 0
            length' [1,2,3] `shouldBe` 3
        it "can reduce add a list" $ do
            sum' [] `shouldBe` 0
            sum' [1,2,3] `shouldBe` 6
        it "can hold the original item with pattern" $ do
            firstLetter "" `shouldBe` "Empty string, whoops!"
            firstLetter "Dracula" `shouldBe` "The first letter of Dracula is D"

