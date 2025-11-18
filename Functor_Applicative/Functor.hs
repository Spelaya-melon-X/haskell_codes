{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative (ZipList(..))
import Data.Monoid (Sum(..), Product(..))

------------------------
-- 1. Пользовательский тип Box
------------------------

data Box a = Box a deriving (Show, Eq)

instance Functor Box where
    fmap f (Box x) = Box (f x)

------------------------
-- 2. Простое дерево
------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

------------------------
-- 3. Вспомогательные функции
------------------------

printSection :: String -> IO ()
printSection name = do
    putStrLn ""
    putStrLn ("========== " ++ name ++ " ==========")

showF :: (Show b) => String -> b -> IO ()
showF name value = putStrLn (name ++ ": " ++ show value)

------------------------
-- 4. Основная демонстрация
------------------------

main :: IO ()
main = do
    ----------------------------------------------------------
    printSection "Functor для Box"
    ----------------------------------------------------------

    let b = Box 10
    showF "Исходный" b
    showF "fmap (+1)" (fmap (+1) b)
    showF "fmap (*3)" (fmap (*3) b)


    ----------------------------------------------------------
    printSection "Functor для Maybe"
    ----------------------------------------------------------

    showF "fmap (+1) (Just 5)" (fmap (+1) (Just 5 :: Maybe Int))
    showF "fmap (+1) Nothing" (fmap (+1) (Nothing :: Maybe Int))


    ----------------------------------------------------------
    printSection "Functor для списка"
    ----------------------------------------------------------

    showF "fmap (*2) [1,2,3]" (fmap (*2) [1,2,3])
    showF "fmap (++\"!\") [\"a\",\"b\"]" (fmap (++"!") ["a","b"])


    ----------------------------------------------------------
    printSection "Functor для ZipList"
    ----------------------------------------------------------

    let z = ZipList [1,2,3]
    showF "fmap (*10) (ZipList [1,2,3])"
        (fmap (*10) z)


    ----------------------------------------------------------
    printSection "Functor для дерева Tree"
    ----------------------------------------------------------

    let t = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
    showF "Исходное дерево" t
    showF "fmap (*10)" (fmap (*10) t)
    showF "fmap show" (fmap show t)


    ----------------------------------------------------------
    printSection "Functor для функций ((->) r)"
    ----------------------------------------------------------

    -- ВНИМАНИЕ: instance уже определён в GHC.Base!
    -- Просто используем его.

    let g = (+2)
    let h = fmap (*10) g      -- (*10) . g

    showF "g 5" (g 5)
    showF "h 5 = fmap (*10) g" (h 5)

    let q = fmap (++"!") show
    showF "q 10 = fmap (++\"!\") show $ 10" (q 10)


    ----------------------------------------------------------
    printSection "Проверка закона функторов"
    ----------------------------------------------------------

    let f = (+1)
    let g2 = (*3)

    showF "fmap id (Box 7)"
        (fmap id (Box 7))

    showF "(fmap (f . g2) (Box 5))"
        (fmap (f . g2) (Box 5))

    showF "((fmap f . fmap g2) (Box 5))"
        ((fmap f . fmap g2) (Box 5))
