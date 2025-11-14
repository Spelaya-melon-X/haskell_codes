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
