get' n xs = helper 0 xs where 
             helper k (x:xs) = if k == n 
                               then x 
                               else helper (k+1) xs

 head' :: [a] -> a
 head' [] = error "empty list"
 head' (x:xs) = x

 last' [] = error "Empty List"
 last' (x:xs) = if xs == [] then x else (last' xs)

 tail' :: [a] -> [a]
 tail' [] = error"Empty list"
 tail' (x:xs) =  xs

 init' :: [a] -> [a]
 init' [] = []
 init' [x] = [] 
 init' (x:xs) = x : (init' xs)

 reverse' :: [a] -> [a]
 reverse' [] = []
 reverse' (x:xs) =reverse' xs ++ [x]

 lenght' :: [a] -> Integer
 lenght' [] = 0
 lenght' (x:xs) = 1 + lenght' xs

 append' :: [a] -> a -> [a]
 append' xs y = xs ++ [y]

 concat' :: [a] -> [a]-> [a]
 concat' x sys = x ++ sys

 drop' n (x:xs) = helper 0 xs where 
                     helper k (x:xs) = if k == n - 1
                               then xs
                               else helper (k+1) xs

 null' :: [a] -> Bool
 null' [] = False 
 null' (xs) = True

 elem' :: (Eq a) => [a] -> a -> Bool
 elem' [] n = False
 elem' (x:xs) n = if x == n then  True  else elem' xs n
