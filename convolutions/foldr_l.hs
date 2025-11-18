import Data.List (unfoldr)

foldR ::  (a-> b -> b ) -> b -> [a] -> b 
foldR f acc [] = acc 
foldR f acc (x:xs)  = f x (foldR f acc xs)

foldL :: (a -> b -> b) -> b -> [a] -> b -- накапливаем получившиеся результат 
foldL f acc [] = acc 
foldL f acc (x : xs) = foldL f (f x acc  ) xs 

-- ------------ EXamples for unfoldr 

-- Функция-генератор
h :: Int -> Int -> Maybe (Int, Int)
h b n = if n > b then Nothing else Just (n, n + 1)

enumFT :: Int -> Int -> [Int]
enumFT a b = unfoldr (h b) a

-- Тестируем:
-- test1 = enumFT 5 10  -- [5,6,7,8,9,10]
-- test2 = enumFT 3 3   -- [3]
-- test3 = enumFT 10 5  -- [] (a > b)


-- ----- Преобразует число в список двоичных цифр (младшие разряды сначала)
binaryDigits :: Int -> [Int]
binaryDigits n =  unfoldr step n
  where
    step 0 = Nothing
    step x = Just (x `mod` 2, x `div` 2)

-- Тестируем:
-- test4 = binaryDigits 13  -- [1,0,1,1] (13 = 1101 в двоичной)


-- examples : 
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

concat' :: [[a]] -> [a]
concat' = foldr (++) []

allOdd :: [Int] -> Bool
allOdd = foldr (\x acc -> odd x && acc) True