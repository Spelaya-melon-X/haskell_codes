
-- ----------------  Simple Enumeration Type: ----------------

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  

dayOfWeek :: DayOfWeek -> String
dayOfWeek Monday = "Monday"
dayOfWeek Tuesday = "Tuesday"
dayOfWeek Wednesday = "Wednesday"
dayOfWeek Thursday = "Thursday"
dayOfWeek Friday = "Friday"
dayOfWeek Saturday = "Saturday"
dayOfWeek Sunday = "Sunday"

-- ---------------- Product Type (Record-like Structure): ----

data Person = Person { name :: String, age :: Int, city :: String }

createPerson :: String -> Int -> String -> Person
createPerson name age city = Person name age city


printPerson :: Person -> String
printPerson (Person name age city) = "Person name: " ++ name ++ ", age: " ++ show age ++ ", city: " ++ city

-- ---------------- Sum Type (Choice between different forms): ---

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = s * (s - a) * (s - b) * (s - c)
    where s = (a + b + c) / 2

-- ---------------- Recursive Data Type (for lists or trees): -- 

data MyList a = Empty | Cons a
instance Show a => Show (MyList a) where
    show Empty = "Empty"
    show (Cons x xs) = "Cons " ++ show x ++ " " ++ show xs

printList :: Show a => MyList a -> String
printList Empty = "Empty"
printList (Cons x xs) = show x ++ " " ++ printList xs

sumList :: Num a => MyList a -> a
sumList Empty = 0
sumList (Cons x xs) = x + sumList xs

find_num :: Eq a => a -> MyList a -> Bool
find_num _ Empty = False
find_num x (Cons y ys) = x == y || find_num x ys

getHead :: MyList a -> Maybe a
getHead Empty = Nothing
getHead (Cons x _) = Just x  

getTail :: MyList a -> MyList a
getTail Empty = Empty
getTail (Cons _ xs) = xs   


fromList :: [a] -> MyList a
fromList [] = Empty
fromList (x:xs) = Cons x (fromList xs)




-- ---------------- Main function: ----------------

main = do
    print $ dayOfWeek Monday

    print $ printPerson (createPerson "John" 30 "New York")

    print $ area (Circle 5)
    print $ area (Rectangle 4 6)
    print $ area (Triangle 3 4 5)

