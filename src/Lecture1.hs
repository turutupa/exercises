{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module                  : Lecture1
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 1 of the Haskell Beginners course.
--
-- To complete exercises, you need to complete implementation and add
-- missing top-level type signatures. You can implement any additional
-- helper functions. But you can't change the names of the given
-- functions.
--
-- Comments before each function contain explanations and example of
-- arguments and expected returned values.
--
-- It's absolutely okay if you feel that your implementations are not
-- perfect. You can return to these exercises after future lectures and
-- improve your solutions if you see any possible improvements.
module Lecture1
  ( makeSnippet,
    sumOfSquares,
    lastDigit,
    minmax,
    subString,
    strSum,
    lowerAndGreater,
  )
where

-- | Specify the type signature of the following function. Think about
-- its behaviour, possible types for the function arguments and write the
-- type signature explicitly.
makeSnippet :: Int -> String -> String
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

-- | Implement a function that takes two numbers and finds sum of
-- their squares.
--
-- >>> sumOfSquares 3 4
-- 25
--
-- >>> sumOfSquares (-2) 7
-- 53
--
-- Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
-- is 25.

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x * x + y * y

-- headOrDefault :: Int -> [Int] -> Int
-- headOrDefault def list
--     | null list = def
--     | otherwise = head list

-- sameThreeAround :: [Int] -> Bool
-- sameThreeAround list =
--     let firstThree = take 3 list
--         lastThree  = reverse(take 3 (reverse list))
--     in firstThree == lastThree

-- sameThreeAround :: [Int] -> Bool
-- sameThreeAround list = take 3 list == lastThree list
--     where
--         lastThree :: [Int] -> [Int]
--         lastThree list = reverse(take 3 (reverse list))

-- appendLastTwo list1 list2 = lastTwo list1 ++ lastTwo list2
--     where
--         lastTwo :: [Int] -> [Int]
--         lastTwo list = reverse ( take 2 (reverse list))

countOcurrances :: Int -> [Int] -> Int
countOcurrances n = go 0
  where
    go :: Int -> [Int] -> Int
    go result l
      | null l = result
      | head l == n = go (result + 1) (tail l)
      | otherwise = go result (tail l)

-- | Implement a function that returns the last digit of a given number.
--
-- >>> lastDigit 42
-- 2
-- >>> lastDigit (-17)
-- 7
--
-- 🕯 HINT: use the @mod@ functionn

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10

-- | Write a function that takes three numbers and returns the
-- difference between the biggest number and the smallest one.
--
-- >>> minmax 7 1 4
-- 6
--
-- Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
-- and 1 is the smallest, and 7 - 1 = 6.
--
-- Try to use local variables (either let-in or where) to implement this
-- function.
minmax :: Int -> Int -> Int -> Int
minmax x y z = max x (max y z) - min x (min y z)

-- minmax x y z =
--     let maxNum = max x (max y z)
--         minNum = min x (min y z)
--     in maxNum - minNum

-- satisfies :: (Int -> Bool) -> Int -> String
-- satisfies check n
--     | check n   = "The number " ++ show n ++ " passes check"
--     | otherwise = "The number " ++ show n ++ " doesn't pass"

-- doublePlusOne :: (Int -> Int) -> [Int] -> [Int]
-- doublePlusOne f x = map f x

-- | Implement a function that takes a string, start and end positions
-- and returns a substring of a given string from the start position to
-- the end (including).
--
-- >>> subString 3 7 "Hello, world!"
-- "lo, w"
--
-- >>> subString 10 5 "Some very long String"
-- ""
--
-- This function can accept negative start and end position. Negative
-- start position can be considered as zero (e.g. substring from the
-- first character) and negative end position should result in an empty
-- string.
subString :: Int -> Int -> [Char] -> [Char]
subString start end str =
  if end < 0
    then []
    else reverse (drop (max 0 (length str - end - 1)) (reverse body))
  where
    body = drop (max 0 start) str

-- | Write a function that takes a String — space separated numbers,
-- and finds a sum of the numbers inside this string.
--
-- >>> strSum "100    -42  15"
-- 73
--
-- The string contains only spaces and/or numbers.
strSum :: [Char] -> Int
strSum str = sumList (parseIntList (words str)) 0

parseIntList :: [[Char]] -> [Int]
parseIntList list = map read list :: [Int]

sumList :: [Int] -> Int -> Int
sumList list counter =
  if null list
    then counter
    else sumList (tail list) (counter + head list)

-- | Write a function that takes a number and a list of numbers and
-- returns a string, saying how many elements of the list are strictly
-- greater than the given number and strictly lower.
--
-- >>> lowerAndGreater 3 [1 .. 9]
-- "3 is greater than 2 elements and lower than 6 elements"
--
-- Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- The given number 3 is greater than 2 elements (1 and 2)
-- and lower than 6 elements (4, 5, 6, 7, 8 and 9).
--
-- 🕯 HINT: Use recursion to implement this function.
lowerAndGreater :: Int -> [Int] -> [Char]
lowerAndGreater n list = go n list [] []
  where
    go :: Int -> [Int] -> [Int] -> [Int] -> [Char]
    go n list greater lower
      | null list =
        show n
          ++ " is greater than "
          ++ show (length lower)
          ++ " elements and lower than "
          ++ show (length greater)
          ++ " elements"
      | head list > n = go n (tail list) (greater ++ [head list]) lower
      | head list == n = go n (tail list) greater lower
      | otherwise = go n (tail list) greater (lower ++ [head list])
