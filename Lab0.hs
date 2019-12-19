-- 1
get' :: [a] -> Integer -> a
get' [] _ = error "Empty List"
get' (x:xs) n = if n > 0 then  get' (xs) (n-1) else (x)

-- 2
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

-- 3
last' :: [a] -> a
last' [] = error "Empty list"
last' (x:[]) = x
last' (x:xs) = last xs

-- 4
tail' :: [a] -> [a]
tail' [] = error"Empty list"
tail' (x:xs) =  xs

--5
init' :: [a] -> [a]
init' [] = []
init' [x] = [] 
init' (x:xs) = x : (init' xs)

--6

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = helper xs [] where
              helper [] acc = acc
              helper (x:xs) acc = helper xs (x:acc)
             
--reverse' :: [a] -> [a]
--reverse' xs = foldl (\x y -> y:x) [] xs 

--7
length':: [a] -> Integer
length' xs = helper 0 xs where 
             helper n [] = n
             helper n (x:xs) = helper (n+1) xs

-- 8
 -- Есть еще такой вариант, выбирайте какой вам больше нравится)
 --   append' :: Eq a => [a] -> a -> [a]
 --   append' xs x = reverse' (x : reverse' xs)
 
append' :: [a] -> a -> [a]
append' [] y =  y:[]
append' (x:xs) y  =  x : (append' xs y) 

-- 9
concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : (concat' xs ys)  

-- 10
drop' :: Integer -> [a] -> [a]
drop' 0 xs   =   xs
drop' _ []   =   []
drop' n (x:xs)  | (n > 0)  = drop' (n-1) xs 
                | otherwise  = error "Not correct"
               
 -- 11
take' :: Integer->[a]->[a]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x : (take' (n-1) xs)

-- 12
splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' n xs  | (length xs < n ) = error "n is bigger than lenght of list"
               | otherwise = (take (n-1) (xs), drop (n-1) (xs))
 
-- 13
null' :: [a] -> Bool
null' [] = False 
null' (xs) = True

-- 14
elem' :: (Eq a) => [a] -> a -> Bool
elem' [] n = False
elem' (x:xs) n = if x == n then  True  else elem' xs n

--15
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  =  []
filter' test (x:xs)  | (test x == True)  =  x:(filter' test xs)
                     | otherwise   =  filter' test xs

--16
map' :: (a -> b) -> [a] -> [b]
map' f []      =  []
map' f (x:xs)  =  (f x):(map' f xs)                    
 
 -- 17
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
