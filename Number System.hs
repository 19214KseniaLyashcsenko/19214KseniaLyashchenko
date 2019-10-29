import Data.Char
 
toDecimal :: Int -> String -> String
toDecimal base snumber = if (base <= 1 || base > 62) then error "Incorrect base" 
                      else helper base snumber  0
                      where 
                      helper b [] c = show c
                      helper b s c = helper b (tail s) (b * c + number (head s))
                      number x =  if x >= '0' && x <= '9' then (ord x - 48)
                                  else if x >= 'a' && x <= 'z' then (ord x - 87)
                                  else (ord x - 55)

fromDecimal:: Int -> String -> String
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