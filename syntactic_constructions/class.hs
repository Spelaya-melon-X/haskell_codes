-- Define a type class called 'Measurable'
class Measurable a where
  size :: a -> Integer
  diameter :: a -> Integer

data Box = Box { width :: Integer, height :: Integer, depth :: Integer } deriving (Show) -- куб 
data Sphere = Sphere { radius :: Integer } deriving (Show) -- сфера 


instance Measurable Box where --  обхъем куба 
  size (Box w h d) = w * h * d

  diameter (Box w h d) = 2 * max w (max h d)


instance Measurable Sphere where -- объем сферы
  size (Sphere r) = 4 * r * r * r `div` 3

  diameter (Sphere r) = 2 * r


-- EQUIVALENCE -- 

class MyEquiv a where
  (===), (=/=) :: a -> a -> Bool
  x =/= y = not (x === y)
  -- Не делай обратное определение чтобы избежать цикла!

data MyType = A | B | C

instance MyEquiv MyType where
  A === A = True
  B === B = True
  C === C = True
  _ === _ = False

-- Использование:
-- A === A → True
-- A =/= B → True


-- instance example 
data Point = Point { x :: Int, y :: Int }

instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2
    p1 /= p2 = not (p1 == p2)





main :: IO ()
main = do
  let myBox = Box 2 3 4
  let mySphere = Sphere 5

  putStrLn $ "Size of myBox: " ++ show (size myBox)
  putStrLn $ "Size of mySphere: " ++ show (size mySphere)