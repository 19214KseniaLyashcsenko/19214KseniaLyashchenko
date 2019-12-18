import Data.Char
 
toDecimal :: Int -> String -> String
toDecimal 1 snumber = 
toDecimal base snumber = if (base <= 1 || base > 62) then error "Incorrect base" 
                      else if any (\x -> (number x) >= base) snumber then error "Wrong number"
                      else show (foldl (\num x -> num * base + number x ) 0 snumber)
                      where 
                      number x =  if x >= '0' && x <= '9' then (ord x - 48)
                                  else if x >= 'a' && x <= 'z' then (ord x - 87)
                                  else (ord x - 55)

fromDecimal:: Int -> String -> String
fromDecimal 1 snumber = helper (read snumber::Int) []
                        where helper 0 num = num
                              helper snumber num = helper (snumber - 1) ('1':num)

fromDecimal toBase snumber = if toBase > 61 || toBase < 1 then error "Wrong Base" 
                             else helper toBase (read snumber::Int) []
                                   where 
                                   helper b 0 a = a
                                   helper b n a = helper b (n `div` b) ( chr (number (n `mod` b)):a)
                                   number x = if (x <= 9) then x + 48
                                   else if (x <= 35) then x + 87
                                   else x + 55

convertFromTo:: Int -> Int -> [Char] -> [Char]
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
