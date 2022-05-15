module Lib where


maxN :: Int
maxN = 10

primes :: [Int]
primes = [2,3,5,7]

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

-- multiple ways to handle errors --

myHead :: [a] -> a 
myHead [] = error "empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n - 1) (Just (tail xs))

-- Intro To either type --

eitherHead :: [a] -> Either String a
eitherHead [] = Left "Error, empty list has no head"
eitherHead (x:_) = Right x 

-- you can create your own error types which is very powerful --

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
     show TooLarge = "value exceeds max bound"
     show InvalidValue = "value is not a candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue
    | n > maxN = Left TooLarge
    |otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError 