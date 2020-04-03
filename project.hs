year d1 m1 y1 d2 m2 y2 | (m2 > m1) = y2 - y1
                       | (m2 == m1 && d2 > d1 ) = y2 - y1
                       | otherwise = y2 - y1 - 1

time d1 m1 y1 d2 m2 y2 = fromIntegral(year d1 m1 y1 d2 m2 y2  * 365 +vesYear (y1+1) (y2-1) 0 1+ days d1 m1 y1 d2 m2 y2 )
  where vesYear y1 y2 k 0 = k
        vesYear y1 y2 k f = vesYear (y1 + 1) y2 (if ves y1 == 1 then k + 1 else k) (if y1 == y2 then f - 1 else f)

days d1 m1 y1 d2 m2 y2 =  if (m1 >= m2) then (kday1 + kday2 )  else  kday3 
   where kday1 = (if (n1 /= 0) then ((fromList m1 (y2 - 1)) - d1 + foldl (\x y -> x + y) 0 [  (fromList r (y2 - 1)) | r <- [n1..12]]) else ((fromList m1 (y2 - 1)) - d1)) 
         kday2 = (if (n2 /= 0) then (d2 + foldl (\x y -> x + y) 0 [  (fromList r y2) | r <- [1..n2]]) else d2)
         kday3 = if (m2 - m1) > 1 then  (foldl (\x y -> x + y) 0 [  (fromList r y2) | r <- [n1..n2]] + ((fromList m1 y2) - d1) + d2 ) else ((fromList m1 y2) - d1) + d2
         n1 = if (m1 + 1 > 12) then 0 else m1 + 1
         n2 = if (m2 - 1 == 0) then 0 else m2 - 1

fromList m y = (if (m == 2) then (if (ves y == 1) then 29 else 28 ) else monthsInt !! m)       

phis d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 23 )* 100 )

emotional d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 28 )* 100 )

mental d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 33 )* 100 )

dayInWeek d m y = (i_y + i_m + d + (if (y `div` 1000 == 2) then  0 else 1) + (if (y `mod` 4 == 0 && m == 1 || y `mod` 4 == 0 && m == 2)then -1 else 0)) `mod` 7
 where i_m | (m == 1) || (m == 10) = 6
           | (m == 2) || (m == 3) || (m == 11) = 2
           | (m == 4) || (m == 7) = 5
           | (m == 5) = 0
           | (m == 6) = 3
           | (m == 8) = 1
           | (m == 9) || (m == 12) = 4
       i_y = (x `div` 12) + (x `mod` 12) + ((x `mod` 12) `div` 4)
         where x = y `mod` 100

ves year = if (year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0) then 1 else 0

monthsInt = [0,31,28,31,30,31,30,31,31,30,31,30,31]
