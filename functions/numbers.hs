
abs_ :: (Num a , Ord a) => a -> a 
abs_ x = if x > 0 then x else -x

signum_ :: (Num a , Ord a) => a -> a 
signum_ x = if x > 0 then 1 else if x < 0 then -1 else 0

negate_ :: Num a => a -> a 
negate_ x = -x

recip_ :: Fractional a => a -> a 
recip_ x = 1/x

even_ :: Integral a => a -> Bool 
even_ x = x `mod` 2 == 0

odd_ :: Integral a => a -> Bool 
odd_ x = x `mod` 2 /= 0

gcd_ :: (Integral a , Eq a)  => a -> a -> a 
gcd_ a b = if b == 0 then a else gcd_ b (a `mod` b)

lcm_ :: (Integral a , Eq a) => a -> a -> a 
lcm_ a b = div (a * b) (gcd_ a b)

