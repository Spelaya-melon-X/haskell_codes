{-# LANGUAGE OverloadedStrings #-}

import Data.List

--     ---------------------

describeNumber :: Int -> String
describeNumber n = case n of
    0 -> "Zero"
    1 -> "One"
    _ -> "Other" 

--     ---------------------


describeList :: Show a => [a] -> String
describeList xs = case xs of
    [] -> "An empty list"
    [x] -> "A list with one element: " ++ show x
    [x, y] -> "A list with two elements: " ++ show x ++ " and " ++ show y
    (x:xs') -> "A list with at least one element, the head is: " ++ show x

--     ---------------------


describePoint :: (Int, Int) -> String
describePoint p = case p of
    (0, 0) -> "Origin"
    (x, 0) -> "On the X-axis at " ++ show x
    (0, y) -> "On the Y-axis at " ++ show y
    (x, y) -> "General point at (" ++ show x ++ ", " ++ show y ++ ")"

--     ---------------------


signCase :: Int -> Int
signCase x = case x of
    n | n > 0 -> 1
    0 -> 0
    _ -> -1

-- Equivalent using function pattern matching and guards
signGuards :: Int -> Int
signGuards x
    | x > 0     = 1
    | x == 0    = 0
    | otherwise = -1


--     ---------------------

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

processDivision :: Maybe Int -> String
processDivision result = case result of
    Just val -> "Division successful, result: " ++ show val
    Nothing -> "Division by zero occurred!"