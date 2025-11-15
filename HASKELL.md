### Функции для работы со списками



**head** 
> Возвращает первый элемент списка
```haskell
head :: [a] -> a
head [1,2,3]
-- 1
```

**tail** 
> Возвращает список без первого элемента
```haskell
tail :: [a] -> [a]
tail [1,2,3]
-- [2,3]
```

**last** 
> Возвращает последний элемент списка
```haskell
last :: [a] -> a
last [1,2,3]
-- 3
```

**init** 
> Возвращает список без последнего элемента
```haskell
init :: [a] -> [a]
init [1,2,3]
-- [1,2]
```

**null** 
> Проверяет, пуст ли список
```haskell
null :: [a] -> Bool
null []
-- True
```

**length** 
> Возвращает длину списка
```haskell
length :: [a] -> Int
length [1,2,3]
-- 3
```

**elem** 
> Проверяет, содержится ли элемент в списке
```haskell
elem :: Eq a => a -> [a] -> Bool
elem 2 [1,2,3]
-- True
```

**notElem** 
> Проверяет, отсутствует ли элемент в списке
```haskell
notElem :: Eq a => a -> [a] -> Bool
notElem 4 [1,2,3]
-- True
```

**map** 
> Применяет функцию ко всем элементам списка
```haskell
map :: (a -> b) -> [a] -> [b]
map (+1) [1,2,3]
-- [2,3,4]
```

**filter** 
> Фильтрует элементы по условию  
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter even [1,2,3,4]
-- [2,4]
```

**foldl** 
> Сворачивает список слева --> 
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl (+) 0 [1,2,3]
foldl (/) 5 [5 ,1] -- 1.0
-- 6
```

**foldr** 
> Сворачивает список справа <-- 
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (:) [] [1,2,3]
foldr (/) 5 [5 ,1] -- 0.25
-- [1,2,3]
```

**sum** 
> Сумма элементов списка
```haskell
sum :: Num a => [a] -> a
sum [1,2,3]
-- 6
```

**product** 
> Произведение элементов списка
```haskell
product :: Num a => [a] -> a
product [1,2,3,4]
-- 24
```

**reverse** 
> Разворачивает список
```haskell
reverse :: [a] -> [a]
reverse [1,2,3]
-- [3,2,1]
```

**take** 
> Берет первые n элементов
```haskell
take :: Int -> [a] -> [a]
take 2 [1,2,3,4]
-- [1,2]
```

**drop** 
> Удаляет первые n элементов
```haskell
drop :: Int -> [a] -> [a]
drop 2 [1,2,3,4]
-- [3,4]
```

**zip** 
> Объединяет два списка в список пар
```haskell
zip :: [a] -> [b] -> [(a, b)]
zip [1,2] ['a','b']
-- [(1,'a'),(2,'b')]
```

**concat** 
> Объединяет список списков в один список
```haskell
concat :: [[a]] -> [a]
concat [[1,2],[3,4]]
-- [1,2,3,4]
```

---

#### Функции для работы с числами

**abs** 
> Абсолютное значение
```haskell
abs :: Num a => a -> a
abs (-5)
-- 5
```

**signum** 
> Знак числа (-1, 0, 1)
```haskell
signum :: Num a => a -> a
signum (-5)
-- -1
```

**negate** 
> Смена знака
```haskell
negate :: Num a => a -> a
negate 5
-- -5
```

**recip** 
> Обратное число (1/x)
```haskell
recip :: Fractional a => a -> a
recip 4
-- 0.25
```

**fromIntegral** 
> Конвертирует Integral в любой Num ( где `Integral` -*тип-класс*, который определяет операции над целыми числами, включая целочисленное делени)
```haskell
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral (5 :: Int) :: Double
-- 5.0
```

**ceiling** 
> Округление вверх
```haskell
ceiling :: (RealFrac a, Integral b) => a -> b
ceiling 2.3
-- 3
```

**floor** 
> Округление вниз
```haskell
floor :: (RealFrac a, Integral b) => a -> b
floor 2.9
-- 2
```

**round** 
> Математическое округление
```haskell
round :: (RealFrac a, Integral b) => a -> b
round 2.5
-- 3
```

**truncate** 
> Отбрасывание дробной части
```haskell
truncate :: (RealFrac a, Integral b) => a -> b
truncate 2.9
-- 2
```

**even** 
> Проверка на четность
```haskell
even :: Integral a => a -> Bool
even 4
-- True
```

**odd** 
> Проверка на нечетность
```haskell
odd :: Integral a => a -> Bool
odd 3
-- True
```

**gcd** 
> Наибольший общий делитель
```haskell
gcd :: Integral a => a -> a -> a
gcd 12 18
-- 6
```

**lcm** 
> Наименьшее общее кратное
```haskell
lcm :: Integral a => a -> a -> a
lcm 12 18
-- 36
```

**exp** 
> Экспонента (e^x)
```haskell
exp :: Floating a => a -> a
exp 1
-- 2.718281828459045
```

**log** 
> Натуральный логарифм
```haskell
log :: Floating a => a -> a
log 2.718281828459045
-- 1.0
```

**sqrt** 
> Квадратный корень
```haskell
sqrt :: Floating a => a -> a
sqrt 9
-- 3.0
```

**sin, cos, tan** 
> Тригонометрические функции
```haskell
sin :: Floating a => a -> a
sin (pi/2)
-- 1.0
```

**asin, acos, atan** 
> Обратные тригонометрические функции
```haskell
asin :: Floating a => a -> a
asin 1.0
-- 1.5707963267948966
```

### Ключевые слова и конструкции Haskell

**Just**
> `Just` — это конструктор типа `Maybe` в Haskell, который оборачивает значение, чтобы показать, что оно существует. Например, `Just 5` представляет собой значение типа `Int`, а `Nothing` представляет его отсутствие.
```haskell
-- Функция, которая может вернуть число или ничего
safeDivide :: Float -> Float -> Maybe Float
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

result1 = safeDivide 10 2  -- result1 будет `Just 5.0`
result2 = safeDivide 10 0  -- result2 будет `Nothing`

-- Чтобы получить значение из Just, нужно его распаковать
-- Это можно сделать с помощью pattern matching:
extractValue :: Maybe Float -> Float
extractValue (Just val) = val
extractValue Nothing = 0 -- или другое значение по умолчанию

extractValue result1 -- вернет 5.0
extractValue result2 -- вернет 0.0

```

**data** 
> Объявление алгебраического типа данных
```haskell
data Bool = True | False
data Maybe a = Nothing | Just a
-- Создание: True, Just 5, Nothing
```

**newtype** 
> Объявление нового типа с одним конструктором и одним полем (без накладных расходов)
```haskell
newtype UserId = UserId Int
newtype Email = Email String
-- UserId 123 ≠ 123 (разные типы)
```

**type** 
> Создание синонима типа (псевдонима)
```haskell
type String = [Char]
type Matrix a = [[a]]
-- String и [Char] полностью взаимозаменяемы
```

**class** 
> `class` — это *класс типов*, который определяет набор функций, имеющих одинаковые сигнатуры, но разную реализацию для разных типов. Классы типов в Haskell отличаются от классов в  (ООП) и больше похожи на *интерфейсы*, так как описывают _поведение_ для разных типов данных, а не сами данные
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```
и делаем реалзиацию для типа `Int` : 
```haskell
instance Eq Int where
  (==) x y = x == y
  (/=) x y = x /= y
```

**instance** 
> Реализация класса типов для конкретного типа ( мы можем либо юзать внутри deriving либо  определить самому)
```haskell
instance Eq Bool where
    True == True = True
    False == False = True
    _ == _ = False
```

**deriving** 
> Автоматическая генерация реализаций классов
```haskell
data Color = Red | Green | Blue deriving (Show, Eq, Enum)
-- Автоматически создаются show, (==), succ, pred и т.д.
```

**where** 
> Локальные определения внутри функции
```haskell
calculate x y = result
  where
    result = x * y + offset
    offset = 10
```

**let** 
> `let`  — это выражение, используемое для определения локальных переменных и функций внутри другого выражения. Оно позволяет *связать* *имя* с каким-либо значением или функцией, которые затем могут быть использованы только в пределах последующего выражения, следующего за `in`
```haskell
let x = 5 in x * 2
-- 10
```
не трив пример : 
```haskell
-- Быстрая проверка на простое число
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise =
        let limit = floor (sqrt (fromIntegral n)) + 1
            check divisor
                | divisor > limit = True
                | n `mod` divisor == 0 = False
                | otherwise = check (divisor + 2)
        in check 3

isPrime 17
-- True
```

**in** 
> Часть let-выражения, указывающая область видимости
```haskell
let x = 2; y = 3 in x + y
-- 5
```

**do** 
> Синтаксический сахар для монадических вычислений
```haskell
main = do
    putStrLn "Hello"
    name <- getLine
    putStrLn $ "Hi, " ++ name
```

**case** 
> Сопоставление с образцом в выражениях
```haskell
describe x = case x of
    0 -> "Zero"
    1 -> "One"
    _ -> "Other"
```

**of** 
> Часть case-выражения или определения данных
```haskell
data Maybe a = Nothing | Just a  -- of неявный здесь
```

**if** 
> Условное выражение (обязательно с else)
```haskell
abs x = if x >= 0 then x else -x
```

**then** 
> Часть if-выражения
```haskell
if x > 0 then "Positive" else "Non-positive"
```

**else** 
> Альтернативная ветка if-выражения
```haskell
check x = if x == 0 then "Zero" else "Non-zero"
```

**module** 
> Объявление модуля
```haskell
module MyModule where
module Data.List (sort, nub) where  -- экспорт конкретных функций
```

**import** 
> Импорт модулей
```haskell
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
```

**qualified** 
> Квалифицированный импорт ( используется для импорта модулей так, чтобы избежать конфликтов имен между разными модулями)
```haskell
import qualified Data.Set as Set
Set.fromList [1,2,3]  -- вместо fromList [1,2,3]
```

**as** 
> Псевдоним при квалифицированном импорте
```haskell
import qualified Data.ByteString as BS
BS.pack "hello"
```

**hiding** 
> Исключение функций при импорте
```haskell
import Data.List hiding (sort)
```


**infix** 
> Объявление инфиксного оператора с приоритетом 
Это просто *дополнительные правила* для операторов, которые говорят компилятору:
>- В каком порядке выполнять одинаковые операторы
>- Какие операторы выполнять первыми
```haskell
infixl 6 +  -- левоассоциативный, приоритет 6
infixr 5 :  -- правоассоциативный, приоритет 5
```

**infixl** 
> Левоассоциативный инфиксный оператор
```haskell
infixl 6 +  -- 1 + 2 + 3 = (1 + 2) + 3
```

**infixr** 
> Правоассоциативный инфиксный оператор
```haskell
infixr 5 :  -- 1 : 2 : [] = 1 : (2 : [])
```

**foreign** 
> Работа с внешними функциями (FFI)
```haskell
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
```

**default** 
> Установка типов по умолчанию для числовых литералов
```haskell
default (Int, Double)  -- 5 будет Int, 5.0 будет Double
```

**mdo** 
> Рекурсивный do (требует расширения)
```haskell
{-# LANGUAGE RecursiveDo #-}
mdo
    x <- f y
    y <- g x
    return (x, y)
```

**proc** 
> Стрелочная нотация (требует расширения)
```haskell
{-# LANGUAGE Arrows #-}
proc x -> do
    y <- f -< x
    z <- g -< y
    returnA -< z
```

**forall** 
> Явное указание квантора всеобщности
```haskell
{-# LANGUAGE ExplicitForAll #-}
f :: forall a. a -> a
f x = x
```

**family** 
> Объявление семейства типов
```haskell
{-# LANGUAGE TypeFamilies #-}
type family Element c where
    Element [a] = a
    Element (a,b) = a
```

**role** 
> Указание роли параметров типа
```haskell
{-# LANGUAGE RoleAnnotations #-}
type role Map nominal representational
```

**pattern** 
> Объявление образцов (pattern synonyms)
```haskell
{-# LANGUAGE PatternSynonyms #-}
pattern Empty <- [] where Empty = []
```

**static** 
> Статические указатели (для распределенных вычислений)
```haskell
{-# LANGUAGE StaticPointers #-}
static (putStrLn "Hello")
```

**stock** 
> Указание стратегии deriving
```haskell
data T = T deriving stock (Show, Eq)
```

**anyclass** 
> Deriving через любой класс
```haskell
{-# LANGUAGE DeriveAnyClass #-}
data T = T deriving (Show, Generic)
```

**via** 
> Deriving через промежуточный тип
```haskell
{-# LANGUAGE DerivingVia #-}
newtype Meters = Meters Double deriving Num via Double
```

### Deriving стратегии и классы

**deriving Show** 
> Автоматическая генерация функции show для преобразования в строку
```haskell
data Color = Red | Green | Blue deriving Show
show Red
-- "Red"
```

**deriving Eq** 
> Автоматическая генерация проверки на равенство
```haskell
data Color = Red | Green | Blue deriving Eq
Red == Red
-- True
```

**deriving Ord** 
> Автоматическая генерация сравнения (порядок по объявлению)
```haskell
data Priority = Low | Medium | High deriving (Eq, Ord)
Low < High
-- True
```

**deriving Enum** 
> Автоматическая генерация перечисляемых значений
```haskell
data Day = Mon | Tue | Wed deriving Enum
[Mon .. Wed]
-- [Mon, Tue, Wed]
```

**deriving Bounded** 
> Автоматическая генерация минимального и максимального значения
```haskell
data Size = Small | Medium | Large deriving (Enum, Bounded)
minBound :: Size
-- Small
```

**deriving Read** 
> Автоматическая генерация парсера из строки
```haskell
data Color = Red | Green | Blue deriving (Show, Read)
read "Red" :: Color
-- Red
```

**deriving Ix** 
> Автоматическая генерация индексов для массивов
```haskell
data Index = A | B | C deriving (Eq, Ord, Ix, Enum, Bounded)
import Data.Array; array (A, C) [(i, i) | i <- [A..C]]
-- array (A,C) [(A,A),(B,B),(C,C)]
```

**deriving Data** 
> Поддержка обобщенного программирования (Data.Data)
```haskell
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
data Tree a = Leaf a | Node [Tree a] deriving (Show, Data)
```

**deriving Typeable** 
> Поддержка динамических типов
```haskell
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable
data T = T Int deriving Typeable
typeOf T
-- ...
```

**deriving Generic** 
> Поддержка обобщенного представления
```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
data Person = Person { name :: String, age :: Int } deriving Generic
```

**deriving Functor** 
> Автоматическая генерация fmap
```haskell
{-# LANGUAGE DeriveFunctor #-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Functor
fmap (+1) (Leaf 5)
-- Leaf 6
```

**deriving Foldable** 
> Автоматическая генерация сверток
```haskell
{-# LANGUAGE DeriveFoldable #-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Foldable
foldr (+) 0 (Node (Leaf 1) (Leaf 2))
-- 3
```

**deriving Traversable** 
> Автоматическая генерация обхода
```haskell
{-# LANGUAGE DeriveTraversable #-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Traversable
traverse Just (Node (Leaf 1) (Leaf 2))
-- Just (Node (Leaf 1) (Leaf 2))
```

**deriving stock** 
> Явное указание стандартной стратегии deriving
```haskell
data Color = Red | Green | Blue deriving stock (Show, Eq, Ord)
```

**deriving anyclass** 
> Deriving для любого класса (нужны пустые реализации)
```haskell
{-# LANGUAGE DeriveAnyClass #-}
class Printable a where
    printMe :: a -> String
    printMe _ = "default"

data T = T deriving anyclass Printable
printMe T
-- "default"
```

**deriving newtype** 
> Deriving через обертку newtype (наследует от базового типа)
```haskell
{-# LANGUAGE DerivingStrategies #-}
newtype Meters = Meters Double deriving newtype (Show, Num)
Meters 2.5 + Meters 1.5
-- Meters 4.0
```

**deriving via** 
> Deriving через промежуточный тип
```haskell
{-# LANGUAGE DerivingVia #-}
newtype Celsius = Celsius Double 
    deriving (Show, Num) 
    via Double

Celsius 20 + Celsius 5
-- Celsius 25.0
```

**StandaloneDeriving** 
> Отдельное объявление deriving
```haskell
{-# LANGUAGE StandaloneDeriving #-}
data Tree a = Leaf a | Node (Tree a) (Tree a)

deriving instance Show a => Show (Tree a)
deriving instance Eq a => Eq (Tree a)
```

**GeneralizedNewtypeDeriving** 
> Deriving для newtype с параметрами
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
    deriving (Functor, Applicative, Monad)
```

#### **Практические комбинации**

**Базовый набор**:
```haskell
data Color = Red | Green | Blue 
    deriving (Show, Eq, Ord, Enum, Bounded)
```

**Для структур данных**:
```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq, Functor, Foldable, Traversable)
```

**Для newtype**:
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
newtype Email = Email String 
    deriving (Show, Eq) 
    deriving newtype IsString
```

**Полный пример с расширениями**:
```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}
import GHC.Generics

data Result a = Success a | Error String
    deriving (Show, Eq, Functor, Generic, NFData)
```



### Синтаксические конструкции и операторы Haskell

**| (вертикальная черта)** 
> Разделение конструкторов в data или альтернатив в case
```haskell
data Maybe a = Nothing | Just a
case x of
    0 -> "Zero"
    1 -> "One"
    | otherwise -> "Other"
```

**$ (оператор применения)** 
> Правоассоциативное применение функции (убирает скобки)
```haskell
f $ g $ h x  -- эквивалентно f (g (h x))
sum $ filter even [1..10]
-- 30
```

**. (композиция функций)** 
> Композиция двух функций
```haskell
(f . g) x = f (g x)
length . filter even $ [1..10]
-- 5
```

**\ (лямбда)** 
> Анонимные функции
```haskell
\x -> x + 1
map (\x -> x * 2) [1,2,3]
-- [2,4,6]
```

**:: (сигнатура типа)** 
> Указание типа выражения
```haskell
5 :: Int
map :: (a -> b) -> [a] -> [b]
```

**-> (стрелка типа)** 
> Тип функции или в паттерн матчинге
```haskell
Int -> String  -- функция из Int в String
case x of
    Just y -> show y
    Nothing -> "empty"
```

**@ (as-паттерн)** 
> Связывание значения и паттерна одновременно
```haskell
first@(x:xs) = [1,2,3]
-- first = [1,2,3], x = 1, xs = [2,3]
```

**~ (ленивый паттерн)** 
> Откладывает сопоставление до фактического использования
```haskell
f ~(x,y) = x + 1
f undefined  -- не падает сразу
```

**! (строгий паттерн)** 
> Форсирует вычисление при сопоставлении
```haskell
data StrictPair = SP !Int !Int
f (SP x y) = x + y  -- x и y вычисляются сразу
```

**# (unboxed типы)** 
> Примитивные неупакованные типы
```haskell
data Int = I# Int#  -- внутри компилятора
```

**.. (диапазон)** 
> Генерация списков по диапазону
```haskell
[1..5]      -- [1,2,3,4,5]
[1,3..10]   -- [1,3,5,7,9]
['a'..'c']  -- "abc"
```

**\\ (list comprehension)** 
> Генераторы списков
```haskell
[x * 2 | x <- [1..3], even x]
-- [4]  (только 2*2)
```

**<- (привязка в монадах)** 
> Извлечение значения из монадического контекста
```haskell
do
    x <- getLine
    return (x ++ "!")
```

**-> (в case/of)** 
> Связывание паттерна с выражением
```haskell
case x of
    Just val -> show val
    Nothing -> "none"
```

**, (кортеж)** 
> Создание кортежей
```haskell
(1, "hello") :: (Int, String)
fst (1,2)  -- 1
snd (1,2)  -- 2
```

**; (разделитель)** 
> Разделение выражений (редко используется)
```haskell
let x = 1; y = 2 in x + y
-- 3
```

**` (обратные кавычки)** 
> Использование функции как инфиксного оператора
```haskell
5 `div` 2  -- эквивалентно div 5 2
-- 2
```

**' (апостроф)** 
> Имена с апострофом (обычно для строгих версий)
```haskell
foldl'  -- строгая версия foldl
x'      -- переменная с апострофом
```

**_ (wildcard)** 
> Игнорирование значения в паттерн матчинге
```haskell
(_, y) = (1, 2)  -- игнорируем первый элемент
map _ [1,2,3]    -- частичное применение
```

**? (в шаблонах)** 
> Char и string literals (GHC расширение)
```haskell
{-# LANGUAGE OverloadedStrings #-}
?'a'  -- символ как значение
```

**# (в шаблонах)** 
> Literal patterns (GHC расширение)
```haskell
{-# LANGUAGE MagicHash #-}
case x of
    0# -> "zero"
    _  -> "other"
```

**: (конструктор списка)** 
> Добавление элемента в начало списка
```haskell
1 : [2,3]  -- [1,2,3]
'x' : "yz" -- "xyz"
```

**++ (конкатенация)** 
> Объединение списков
```haskell
[1,2] ++ [3,4]  -- [1,2,3,4]
"hello" ++ " world"
-- "hello world"
```

**!! (индекс)** 
> Доступ к элементу списка по индексу
```haskell
"hello" !! 1  -- 'e'
[10,20,30] !! 2
-- 30
```

**\\ (разность списков)** 
> Удаление элементов из списка
```haskell
[1,2,3,4] \\ [2,4]  -- [1,3]
```

**^ (возведение в степень)** 
> Целочисленное возведение в степень
```haskell
2 ^ 3  -- 8
```

**^^ (дробное возведение)** 
> Дробное возведение в степень
```haskell
4 ^^ (-2)  -- 0.0625
```

**(умножение)** 
> Умножение чисел
```haskell
3 * 4  -- 12
```

**/ (деление)** 
> Дробное деление
```haskell
7 / 2  -- 3.5
```

**`div` (целочисленное деление)** 
> Целочисленное деление
```haskell
7 `div` 2  -- 3
```

**`mod` (остаток от деления)** 
> Остаток от деления
```haskell
7 `mod` 2  -- 1
```

**== (равенство)** 
> Проверка на равенство
```haskell
5 == 5  -- True
```

**/= (неравенство)** 
> Проверка на неравенство
```haskell
5 /= 3  -- True
```

**< > <= >= (сравнение)** 
> Операторы сравнения
```haskell
3 < 5   -- True
5 >= 5  -- True
```

**&& (логическое И)** 
> Логическое И
```haskell
True && False  -- False
```

**|| (логическое ИЛИ)** 
> Логическое ИЛИ
```haskell
True || False  -- True
```

**not (логическое НЕ)** 
> Логическое отрицание
```haskell
not True  -- False
```

**>>= (bind)** 
> Оператор связывания в монадах
```haskell
Just 5 >>= \x -> Just (x + 1)
-- Just 6
```

**>> (then)** 
> Последовательное выполнение в монадах
```haskell
putStr "Hello" >> putStrLn " World"
-- Hello World
```

**$<*>$ (ap)** 
> Applicative применение
```haskell
Just (+1) <*> Just 5  -- Just 6
```

**<$> (fmap)** 
> Инфиксная версия fmap
```haskell
(+1) <$> [1,2,3]  -- [2,3,4]
```

**<|> (альтернатива)** 
> Выбор в Alternative
```haskell
Nothing <|> Just 5 <|> Just 10
-- Just 5
```


**<&> (fan-out)**  
> Оператор `<&>` — это **реверсивная версия** `<$>` (fmap). Он применяет функцию к значению внутри функтора, но **в обратном порядке**: сначала идёт значение, потом — функция.  
```haskell
[1,2,3] <&> (+1)  -- [2,3,4]
```
> Аналогично: `x <&> f` эквивалентно `fmap f x`, но с инвертированным порядком аргументов.  
> Полезен для композиции и улучшения читаемости в цепочках.  
> Часто используется в библиотеках, таких как `lens` или `relude`.



### еще фукнции 

**`rotate`**  
> Функция `rotate` не входит в стандартную библиотеку, но часто реализуется как сдвиг списка на `n` позиций.  
```haskell
import Data.List (drop, take)
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
-- rotate 2 [1,2,3,4,5] == [3,4,5,1,2]
```

---

**`all`**  
> Проверяет, удовлетворяют ли **все** элементы предикату.  
```haskell
all (>3) [4,5,6]  -- True
all (>3) [4,2,6]  -- False
```

---

**`scanl`**  
> Аналог `foldl`, но возвращает **промежуточные результаты**.  
```haskell
scanl (+) 0 [1,2,3]  -- [0,1,3,6]
```

---

**`#`**  
> Оператор `#` часто используется в библиотеках, таких как `lens`, как **реверсивный `$`**, то есть `x # f == f x`.  
> Также может использоваться в `DataKinds` и `TypeOperators` для определения типов.  

---

**`scanr`**  
> Аналог `foldr`, но возвращает **промежуточные результаты** справа.  
```haskell
scanr (+) 0 [1,2,3]  -- [6,5,3,0]
```

---

**`unfoldr`**  
> Дуал к `foldr`: **строит список** из начального значения и функции.  
```haskell
unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 5
-- [5,4,3,2,1]
```

---

**`iterate`**  
> Создаёт бесконечный список, применяя функцию к начальному значению.  
```haskell
take 5 $ iterate (*2) 1  -- [1,2,4,8,16]
```

---

**`Nothing`**  
> Один из двух конструкторов типа `Maybe a`, обозначающий **отсутствие значения**.  
```haskell
headMay [] = Nothing
```

---

**`build`**  
> Внутренняя функция GHC, используемая для оптимизации списков.  
> Применяется в `GHC.Exts` для построения списков с помощью свёрток.  

---

**`forall`**  
> Является **квантором всеобщности** в типах (RankNTypes).  
> Позволяет создавать полиморфные функции.  
```haskell
{-# LANGUAGE RankNTypes #-}
type Id = forall a. a -> a
```

---

**`stimes`**  
> Эффективное **возведение в степень** для моноидов/групп.  
> `stimes n x = x <> x <> ... <> x` (n раз).  

---

**`sconcat`**  
> Ассоциативная операция для `Semigroup`:  
> `sconcat (a :| [b, c]) = a <> b <> c`.  

---

**`mappend`**  
> Синоним для `(<>)` в `Monoid`.  
> Комбинирует два значения моноида.  
```haskell
mappend [1,2] [3,4]  -- [1,2,3,4]
```

---

**`mempty`**  
> Нейтральный элемент в `Monoid`.  
```haskell
mempty :: [Int]  -- []
```

---

**`Monoid`**  
> Класс типов с ассоциативной бинарной операцией `mappend` и нейтральным элементом `mempty`.  
```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

---

**`Semigroup`**  
> Класс типов с ассоциативной бинарной операцией `<>`.  
> `Monoid` — это `Semigroup` + `mempty`.  

---

**`class Foldable`**  
> Класс типов, представляющий структуры, которые можно **свёртывать**.  
> Определяет `foldr`, `foldMap`, `toList` и др.  
```haskell
length :: Foldable t => t a -> Int
```

---

### Структуры и конструкции условий в Haskell

**`if / then / else`**  
> Выражение условного ветвления. Всегда требует `else`.  
```haskell
if x > 0 then "positive" else "negative or zero"
```

**`if / then Nothing else Just x`**  
> Используется для возврата `Maybe a` в зависимости от условия.  
```haskell
safeDiv x y = if y == 0 then Nothing else Just (x `div` y)
```

**`case`**  
> Паттерн-матчинг. Более общий способ ветвления.  
```haskell
case xs of
  [] -> "empty"
  (y:ys) -> "non-empty"
```

**`let / in`**  
> Локальное определение переменных.  
```haskell
let x = 2 in x * x
```

**`where`**  
> Определение переменных после тела функции.  
```haskell
squareSum x y = a + b
  where a = x^2
        b = y^2
```

**`do`**  
> Синтаксис для работы с монадами.  
```haskell
main = do
  putStrLn "Hello"
  putStrLn "World"
```

