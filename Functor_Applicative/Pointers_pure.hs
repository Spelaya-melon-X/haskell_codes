{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative (ZipList(..))
import Data.Monoid (Sum(..), Product(..))

-------------------------
-- Наш учебный Pointed
-------------------------

-- Это НЕ настоящий класс Haskell, просто иллюстрация.
class Functor f => Pointed f where
    point :: a -> f a
    point = pureLike

    -- Позволяем переопределять "вставку значения"
    pureLike :: a -> f a
    pureLike = point

-------------------------
-- Box — пользовательский тип
-------------------------

data Box a = Box a deriving (Show, Eq)

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Pointed Box where
    point x = Box x

-------------------------
-- Простое бинарное дерево
-------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Pointed Tree where
    point = Leaf         -- вставка значения как одиночный лист

-------------------------
-- Обёртка для функций
-------------------------

-- Для функций Pointed смысла не имеет (нет "контейнера"),
-- но для демонстрации введём фиктивный пример:

newtype WrapFun r a = WrapFun { getFun :: r -> a }

instance Functor (WrapFun r) where
    fmap f (WrapFun g) = WrapFun (f . g)

instance Pointed (WrapFun r) where
    -- Константная функция r -> x
    point x = WrapFun (\_ -> x)

-------------------------
-- Печать секций
-------------------------

printSection :: String -> IO ()
printSection name = do
    putStrLn ""
    putStrLn ("========== " ++ name ++ " ==========")

showF name val = putStrLn (name ++ ": " ++ show val)

-------------------------
-- MAIN
-------------------------

main :: IO ()
main = do

    ------------------------------------------------------------------------
    printSection "Pointed для Box"
    ------------------------------------------------------------------------

    showF "point 10 :: Box Int"
        (point 10 :: Box Int)

    showF "fmap (+1) (point 10)"
        (fmap (+1) (point 10 :: Box Int))


    ------------------------------------------------------------------------
    printSection "Pointed для Tree"
    ------------------------------------------------------------------------

    showF "point 7 :: Tree Int"
        (point 7 :: Tree Int)

    showF "fmap (*10) (point 7)"
        (fmap (*10) (point 7 :: Tree Int))


    ------------------------------------------------------------------------
    printSection "Pointed для функции (WrapFun)"
    ------------------------------------------------------------------------

    let w = point 42 :: WrapFun String Int
    showF "getFun (point 42) \"abc\""
        (getFun w "abc")

    let w2 = fmap (+100) w
    showF "getFun (fmap (+100) (point 42)) \"zzz\""
        (getFun w2 "zzz")


    ------------------------------------------------------------------------
    printSection "Pointed для Maybe / List / ZipList (через Applicative)"
    ------------------------------------------------------------------------

    -- Поскольку в Haskell настоящего Pointed нет, мы показываем аналогию через pure:

    showF "pure 3 :: Maybe Int" (pure 3 :: Maybe Int)
    showF "pure 3 :: [Int]"     (pure 3 :: [Int])
    -- showF "pure 3 :: ZipList Int" (pure 3 :: ZipList Int) -- бесконечное число элементов 
    print . take 10 . getZipList $ pure 3 -- тут мы выводим первые 10 


    ------------------------------------------------------------------------
    printSection "Главный закон Pointed:"
    putStrLn "  fmap g . point  ==  point . g"
    putStrLn ""

    let g = (*10)
    let x = 7

    -- Левая часть: fmap g (point x)
    let left  = fmap g (point x :: Box Int)
    -- Правая часть: point (g x)
    let right = point (g x) :: Box Int

    showF "fmap g (point 7)" left
    showF "point (g 7)" right

    putStrLn $ "Левое == правое? " ++ show (left == right)
