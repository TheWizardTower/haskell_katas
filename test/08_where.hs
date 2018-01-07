import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- BMI boundaries are 18.5, 25.0 and 30.0
-- Calculation logic: weight / height ^ 2
bmiTell :: Num a => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal."
    | bmi <= fat    = "You're fat! Lose some weight!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / (height ^ 2)
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

initials :: String -> String -> String
initials [] [] = ""
initials (x:xs) [] = [x]
initials [] (y:ys) = [y]
initials (x:xs) (y:ys) = (x:y)

calcBmis :: Fractional t => [(t, t)] -> [t]
calcBmis (w,h):xs = (w / (h ^ 2)) : calcBmis xs

main :: IO()
main = hspec $ do
    describe "where - to DRY up logic" $ do
        it "can calculate BMI from values" $ do
            pending
            bmiTell 85 1.90 `shouldBe` "You're supposedly normal."
        it "can extract initials from a string" $ do
            initials "" "" `shouldBe` ""
            initials "Attila" "Domokos" `shouldBe` "AD"
        it "can be used in list comprehensions" $ do
            calcBmis [(85, 1.90)] `shouldBe` [23.545706371191137]
