
-- if then else

calculateBonus :: Double -> Double -> Double
calculateBonus salary performanceRating =
  if performanceRating > 0.8 then salary * 0.10 else salary * 0.05

--  if then else if - then - else 

signum' :: Int -> Int
signum' x = if x > 0 then 1 else if x < 0 then -1 else 0


-- Example with guards (often preferred for multiple conditions):
signumGuard :: Int -> Int
signumGuard x
  | x > 0     = 1
  | x < 0     = -1
  | otherwise = 0



