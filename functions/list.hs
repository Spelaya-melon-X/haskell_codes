

head_ :: [a] -> a
haed_ [] = error "empty list"
head_ (x:_) = x


tail_ :: [a] -> [a]
tail_ [] = error "empty list"
tail_ (_:xs) = xs


last_ :: [a] -> a
last_ [] = error "empty list"
last_ [x] = x
last_ (_ : xs) = last_ xs 

init_ :: [a] -> [a]
init_ [] = error "empty list"
init_ [x] = []
init_ (x : xs) = x : init_ xs 


null_ :: [a] -> Bool 
null_ [] = True
null_ _ = False

length_ :: [a] -> Int
length_ [] = 0
length_ (_ : xs) =  length_ xs + 1


elem_ :: Eq a => a -> [a] -> Bool 
elem_ _ [] = False 
elem_ x (y : ys) 
    | x == y = True 
    | otherwise = elem_ x ys



not_elem_ :: Eq a => a -> [a] -> Bool 
not_elem_ x xs = not ( elem_ x xs ) 



map_ :: (a -> b ) -> [a] -> [b]  -- (a -> b) - фукнция , [a] - список , [b] - список
map_ _ [] = []
map_ f (x : xs) = f x : map f xs 


filter_ :: ( a -> Bool) -> [a] -> [a]
filter_ _ [] = []
filter_ f ( x : xs )
    | f x = x : filter_ f xs -- f x == True 
    | otherwise = filter_ f xs -- f x == False


foldl_ :: (a -> b -> a) -> a -> [b] -> a -- (a -> b -> a) - фукнция , a - начальное значение , [b] - список
foldl_ _ acc []  = acc 
foldl_ f acc ( x : xs ) = foldl_ f ( f acc x ) xs -- у нас тут левая ассоциативность то есть сначала происходит f acc x , потом происходит foldl_ f acc xs , тем самым мы постепенно проходимся по списку и сжимаем слева направо 


foldr_ :: (a -> b -> b) -> b -> [a] -> b -- (a -> b -> b) - фукнция , b - начальное значение , [a] - список
foldr_ _ acc [] = acc 
foldr_ f acc ( x : xs ) = f x ( foldr_ f acc xs ) -- у нас тут правая ассоциативность то есть сначала происходит f x , потом происходит foldr_ f acc xs


sum_ :: Num a => [a] -> a
sum_ [] = 0
sum_ (x : xs) = x + sum_ xs



product_ :: Num a => [a] -> a
product_ [] = 1
product_ (x : xs) = x * product_ xs

reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x : xs) = reverse_ xs ++ [x] -- ++ - конкатенация

take_ :: Int -> [a] -> [a]
take_ 0 _ = []
take_ _ [] = []
take_ n (x : xs) = x : take_ (n - 1) xs


drop_ :: Int -> [a] -> [a] -- она удаляет первые n элементов
drop_ 0 xs = xs
drop_ _ [] = []
drop_ n (x : xs) = drop_ (n-1) xs 

zip_ :: [a] -> [b] -> [(a,b)] 
zip_ [] _ = []
zip_ _ [] = []
zip_ (x : xs) (y : ys) = (x,y) : zip_ xs ys

contact_ :: [a] -> [a] -> [a]
contact_ [] ys = ys
contact_ xs [] = xs
contact_ (x : xs) ys = x : contact_ xs ys



