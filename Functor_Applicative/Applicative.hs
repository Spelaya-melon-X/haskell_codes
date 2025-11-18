{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (ZipList(..))

------------------------------------------------------------
-- 1. Примерная функция для демонстраций
------------------------------------------------------------

add3 :: Int -> Int -> Int
add3 x y = x + y

makePoint :: Int -> Int -> Int -> (Int,Int,Int)
makePoint a b c = (a,b,c)


------------------------------------------------------------
-- 2. Примеры с Maybe
------------------------------------------------------------

exampleMaybe1 :: Maybe Int
exampleMaybe1 = pure add3 <*> Just 10 <*> Just 20
-- Just 30

exampleMaybe2 :: Maybe Int
exampleMaybe2 = pure add3 <*> Just 10 <*> Nothing
-- Nothing


------------------------------------------------------------
-- 3. Примеры со списками []
------------------------------------------------------------

exampleList1 :: [Int]
exampleList1 = pure add3 <*> [1,2] <*> [10,20]
-- [11,21,12,22]


------------------------------------------------------------
-- 4. Примеры с ZipList
------------------------------------------------------------

exampleZip1 :: [Int]
exampleZip1 =
    getZipList (ZipList [add3, add3] <*> ZipList [1,2] <*> ZipList [10,20])
-- [11,22]


------------------------------------------------------------
-- 5. Сравнение семантик на makePoint
------------------------------------------------------------

pMaybe1 :: Maybe (Int,Int,Int)
pMaybe1 = makePoint <$> Just 1 <*> Just 2 <*> Just 3
-- Just (1,2,3)

pMaybe2 :: Maybe (Int,Int,Int)
pMaybe2 = makePoint <$> Just 1 <*> Nothing <*> Just 3
-- Nothing

pList :: [(Int,Int,Int)]
pList = makePoint <$> [1,2] <*> [10] <*> [100,200]
-- [(1,10,100),(1,10,200),(2,10,100),(2,10,200)]

pZip :: [(Int,Int,Int)]
pZip = getZipList $
         makePoint <$> ZipList [1,2,3]
                   <*> ZipList [10,20,30]
                   <*> ZipList [100,200,300]
-- [(1,10,100),(2,20,200),(3,30,300)]


------------------------------------------------------------
-- 6. main: вывод всех примеров
------------------------------------------------------------еуду

main :: IO ()
main = do
    putStrLn "== Maybe =="
    print exampleMaybe1
    print exampleMaybe2

    putStrLn "\n== List =="
    print exampleList1

    putStrLn "\n== ZipList =="
    print exampleZip1

    putStrLn "\n== makePoint comparisons =="
    print pMaybe1
    print pMaybe2
    print pList
    print pZip
