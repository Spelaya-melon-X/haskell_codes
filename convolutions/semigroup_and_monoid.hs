{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Monoid 
import Data.Foldable

import Data.List (maximumBy)


newtype Sum a = Sum { getSum :: a } deriving (Num, Ord, Show)

newtype Max a = Max { getMax :: a } deriving (Ord, Eq)

instance Num a => Num (Max a) where
    (Max x) + (Max y) = Max (max x y)
    (Max x) * (Max y) = Max (x * y)
    fromInteger x = Max (fromInteger x)
    abs (Max x) = Max (abs x)
    signum (Max x) = Max (signum x)
    negate (Max x) = Max (negate x)

instance Semigroup (Max a) where
    (<>) = max

instance Monoid (Max a) where
    mempty = Max { getMax = minBound :: a }
    mappend = (<>)

-- Демонстрация всех инстансов Foldable
main :: IO ()
main = do
    putStrLn "=== Foldable для разных типов ==="
    
    -- 1. Списки
    putStrLn "\n1. Списки:"
    let list = [1,2,3,4]
    putStrLn $ "foldr (+) 0 [1,2,3,4] = " ++ show (foldr (+) 0 list)
    putStrLn $ "foldMap Sum [1,2,3,4] = " ++ show (getSum $ foldMap Sum list)
    
    -- 2. Maybe
    putStrLn "\n2. Maybe:"
    let justVal = Just 42
    let nothing = Nothing :: Maybe Int
    putStrLn $ "foldr (+) 0 (Just 42) = " ++ show (foldr (+) 0 justVal)
    putStrLn $ "foldr (+) 0 Nothing = " ++ show (foldr (+) 0 nothing)
    putStrLn $ "foldMap Sum (Just 42) = " ++ show (getSum $ foldMap Sum justVal)
    
    -- 3. Either
    putStrLn "\n3. Either:"
    let rightVal = Right 100 :: Either String Int
    let leftVal = Left "error" :: Either String Int
    putStrLn $ "foldr (+) 0 (Right 100) = " ++ show (foldr (+) 0 rightVal)
    putStrLn $ "foldr (+) 0 (Left 'error') = " ++ show (foldr (+) 0 leftVal)
    putStrLn $ "foldMap Sum (Right 100) = " ++ show (getSum $ foldMap Sum rightVal)
    
    -- 4. Пары
    putStrLn "\n4. Пары:"
    let pair = ("context", 50)
    putStrLn $ "foldr (+) 0 (\"context\", 50) = " ++ show (foldr (+) 0 pair)
    putStrLn $ "foldMap Sum (\"context\", 50) = " ++ show (getSum $ foldMap Sum pair)
    
    -- 5. Комбинированные примеры
    putStrLn "\n5. Комбинированные примеры:"
    
    -- Список Maybe
    let maybeList = [Just 1, Nothing, Just 3, Just 4]
    putStrLn $ "foldMap (foldMap Sum) [Just 1, Nothing, Just 3, Just 4] = " ++ 
               show (getSum $ foldMap (foldMap Sum) maybeList)
    
    -- Список Either
    let eitherList = [Right 1, Left "err1", Right 3, Right 4]
    putStrLn $ "foldMap (foldMap Sum) [Right 1, Left 'err1', Right 3, Right 4] = " ++ 
               show (getSum $ foldMap (foldMap Sum) eitherList)
    
    -- Список пар
    let pairList = [("a", 1), ("b", 2), ("c", 3)]
    putStrLn $ "foldMap (foldMap Sum) [(\"a\",1), (\"b\",2), (\"c\",3)] = " ++ 
               show (getSum $ foldMap (foldMap Sum) pairList)

-- Практический пример: безопасная обработка данных
processData :: [Either String Int] -> (Sum Int, Max Int)
processData dataList =
    let sums = foldMap (foldMap Sum) dataList
        maxs = foldMap (foldMap Max) dataList
    in (sums, maxs)

-- Тестируем
testProcessing = do
    putStrLn "\n=== Практический пример ==="
    let testData = [Right 10, Left "invalid", Right 20, Right 5, Left "missing"]
    let (totalSum, maxVal) = processData testData
    putStrLn $ "Сумма валидных данных: " ++ show (getSum totalSum)  -- 35
    putStrLn $ "Максимум валидных данных: " ++ show (getMax maxVal) -- 20