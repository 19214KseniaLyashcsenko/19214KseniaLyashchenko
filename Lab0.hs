-- 1
get' :: [a] -> Int -> a
get' [] _ = error "Empty List"
get' (x:xs) n | (n == 0) = x
              | otherwise = get' xs (n - 1)

-- 2
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

-- 3
last' :: Eq a => [a] -> a
last' [] = error "Empty List"
last' (x:xs) = if xs == [] then x else (last' xs)

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
reverse' :: Eq a => [a] -> [a]
reverse'  [] = []
reverse'  xs = last' xs : reverse' (init' xs)

--7
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

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
drop' :: Int -> [a] -> [a]
drop' _ [] = error "Empty List"
drop' n (x:xs) | (length' xs == n) = []
               | (length' xs < n-1) = error "n is bigger then list"
               | (n == 1) = xs
               | otherwise = drop' (n-1) xs
               
 -- 11
take' :: Eq a => Int -> [a] -> [a]
take' 1 (x:xs) = [x]
take' n xs = reverse' (drop' ((length' xs) - n ) (reverse' xs))

-- 12
splitAt' :: Int -> [a] -> ([a],[a])
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
 
 -- 17
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
