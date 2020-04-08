{-# LANGUAGE OverloadedStrings #-}
module Calendar where

import Data.List.Split
import Data.Time.Calendar hiding (Day)
import Text.Read

type Day = Int
type Month = Int
type Year = Integer


checkError1 :: Day -> Month -> Year -> Day -> Month -> Year -> Int
checkError1 d1 m1 y1 d2 m2 y2 = if ((d1 > (if ves y1  == 1 then 29  else  monthsInt !! (m1-1))) || (d2 > (if ves y2  == 1 then 29 else  monthsInt !! (m2-1))) || m1 > 12 || m1 < 1 || m2 > 12 || m2 < 1 || y1 < 1 || y2 < 1 || y1 > y2) then 1 else 0

checkError2 :: Day -> Month -> Year -> Int
checkError2 d1 m1 y1 = if (d1 > (if ves y1  == 1 then 29  else  monthsInt !! (m1-1)) || m1 > 12 || m1 < 1 || y1 < 1 ) then 1 else 0

dayInWeek :: Day -> Month -> Year -> Int
dayInWeek day month year =  ((i_y + i_m + day + (if (year `div` 1000 == 2) then  0 else 1) + (if ((ves year == 1 && month == 1) || (ves year ==1  && month == 2))then -1 else 0)) `mod` 7 )
   where i_m = indexMon !! month
         i_y = fromIntegral ((x `div` 12) + (x `mod` 12) + ((x `mod` 12) `div` 4))
         x = year `mod` 100

time :: Num a => Day -> Month -> Year -> Day -> Month -> Year -> a
time d1 m1 y1 d2 m2 y2 = fromIntegral( (diffDays (fromGregorian y2 m2 d2)(fromGregorian y1 m1 d1)))

ves :: Year -> Int
ves year = if (year `mod` 400 == 0 || year `mod` 4 == 0 && year `mod` 100 /= 0) then 1 else 0

phis :: Day -> Month -> Year -> Day -> Month -> Year -> Int
phis d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 23 )* 100 )

emotional :: Day -> Month -> Year -> Day -> Month -> Year -> Int
emotional d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 28 )* 100 )

mental :: Day -> Month -> Year -> Day -> Month -> Year -> Int
mental d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 33 )* 100 )

months = ["ЯНВАРЯ","ФЕВРАЛЯ"," МАРТА","АПРЕЛЯ","МАЯ","ИЮНЯ","ИЮЛЯ","АВГУСТА","СЕНТЯБРЯ","ОКТЯБРЯ","НОЯБРЯ","ДЕКАБРЯ"]
months2 = ["ЯНВАРЬ","ФЕВРАЛЬ"," МАРТ","АПРЕЛЬ","МАЙ","ИЮНЬ","ИЮЛЬ","АВГУСТ","СЕНТЯБРЬ","ОКТЯБРЬ","НОЯБРЬ","ДЕКАБРЬ"]
days = ["ПОНЕДЕЛЬНИК","ВТОРНИК","СРЕДА","ЧЕТВЕРГ","ПЯТНИЦА","СУББОТА","ВОСКРЕСЕНЬЕ"]
monthsInt = [31,28,31,30,31,30,31,31,30,31,30,31]
indexMon = [0,6,2,2,5,0,3,5,1,4,6,2,4]

dayLine="     | П | В | С | Ч | П | С | В |"
line1 = "\n     "++ concat["+---" | r <- [1..7]]++"+\n"
line2 = "\n     " ++ concat["+---" | r <- [1..7]] ++ "+     " ++ concat["+---" | r <- [1..7]] ++ "+     " ++  concat["+---" | r <- [1..7]] ++ "+\n"
line3 =  concat["+---" | r <- [1..7]] ++ "+     " ++ concat["+---" | r <- [1..7]] ++ "+     " ++  concat["+---" | r <- [1..7]] ++ "+\n"

-- ==============================================================================================================
--                                                  МЕСЯЦОК
-- ==============================================================================================================

monthCalen n = do
 if n == 1 then putStrLn "\n              Введите дату:\n"
 else putStrLn "\n            Попробуй ввести ПРАВИЛЬНО дату)\n"
 str <- getLine

 let st = splitOn " " str
 if ((length st) /= 3) then monthCalen 2 else return()
 if ((readMaybe (st !! 0) :: Maybe Int) == Nothing) then monthCalen 2 else return()
 if ((readMaybe (st !! 1) :: Maybe Int) == Nothing) then monthCalen 2 else return()
 if ((readMaybe (st !! 2) :: Maybe Int) == Nothing) then monthCalen 2 else return()
 let
     d = st !! 0
     m = st !! 1
     y = st !! 2

     year = (read y::Integer)
     month = (read m :: Int)
     day = (read d :: Int)
 if ((checkError2 day month year) == 1) then monthCalen 2 else return()
 let
     mnth = months !! (month-1)
     monthDays = if (month == 2) then (if ves year  == 1 then 29 else 28) else  (monthsInt !! (month-1)) -- колтчество дней в месяце
     firstDay = if (dayInWeek 1 month year == 0) then 7 else dayInWeek 1 month year -- День недели первого числ в месяце

     calendar :: [[Char]] -> [Char]
     calendar cells =  weekLine ++ concat (map render days) ++ "     "++ concat["+---" | r <- [1..7]]++"+\n"
        where days = oneLine cells

     oneLine :: [[Char]] -> [[[Char]]]
     oneLine []=[]
     oneLine (a:b:c:d:e:f:g:remain) = [ [a,b,c,d,e,f,g] ] ++ oneLine remain

     render :: [[Char]] -> [Char]
     render cells =  "     |" ++ (concat $ map renderCell cells) ++ "\n" -- одна строка из дней месяца
        where renderCell str = if ((length str) == 0) then "   |" else if ((length str) == 1) then (" " ++ str ++ " |") else (str ++ " |")

     cells = (replicate n1 "") ++ [ show d | d <- [1..lastday] ] ++ (replicate n2 "") -- список дней ++ пустые ячейки
                where lastday = monthDays
                      n1= firstDay-1
                      n2 = 7 - ((monthDays - (7 - n1)) `mod` 7)

     line1 = "\n     "++ concat["+---" | r <- [1..7]]++"+\n"

     monthLine= "     |   " ++ dw ++ ",  " ++ (show day) ++ " " ++ mnth  ++ (replicate (27 - n) ' ') ++ "|"
                where n = (length dw) + (length (show day)) + (length mnth) + 7
                      dw = days !! ((if (dayInWeek day month year == 0) then 7 else dayInWeek day month year) - 1)

     weekLine = concat[line1,monthLine,line1,dayLine,line1]


 putStrLn (calendar cells)

-- ==============================================================================================================
--                                                  ГОД
-- ==============================================================================================================

yearCalen n = do
  if n == 1 then putStrLn "\n              Введите год:\n"
  else putStrLn "\n            Попробуй ввести ПРАВИЛЬНО год)\n"
  str <- getLine

  let st = splitOn " " str
  if ((length st) /= 1) then yearCalen 2 else return()
  if ((readMaybe (st !! 0) :: Maybe Int) == Nothing) then yearCalen 2 else return()
  let
      year = (read (st !! 0)::Integer)
  if (year < 0) then yearCalen 2 else return()
  let
      monthDays2 :: Int -> Int
      monthDays2 x =  if (x == 2) then (if ves year  == 1 then 29 else 28) else  (monthsInt !! (x-1)) -- колтчество дней в месяце
      firstDay2 :: Int -> Int
      firstDay2 x = if (dayInWeek 1 x year == 0) then 7 else dayInWeek 1 x year -- День недели первого числ в месяце


      calendar =  lineYear1 ++ lineYear2 ++ lineYear3 ++ lineYear4
        where  lineYear1 = weekLine 0 ++ concat (map render (days 1)) ++ "     "++ line3
               lineYear2 = weekLine 3 ++ concat (map render (days 4)) ++ "     "++ line3
               lineYear3 = weekLine 6 ++ concat (map render (days 7)) ++ "     "++ line3
               lineYear4 = weekLine 9 ++ concat (map render (days 10)) ++ "     "++ line3
               days n= (oneLine (cells n) (cells (n + 1)) (cells (n + 2)))

      oneLine :: [[Char]] -> [[Char]] -> [[Char]] -> [[[Char]]]
      oneLine [] [] []= []
      oneLine (a:b:c:d:e:f:g:remain) (a2:b2:c2:d2:e2:f2:g2:remain2) (a3:b3:c3:d3:e3:f3:g3:remain3)= [ [a,b,c,d,e,f,g,"    ",a2,b2,c2,d2,e2,f2,g2,"    ",a3,b3,c3,d3,e3,f3,g3] ] ++ oneLine remain remain2 remain3

      render :: [[Char]] -> [Char]
      render cells =  "     |" ++ (concat $ map renderCell cells) ++ "\n" -- одна строка из дней 3 месяцев
        where renderCell str = if ((length str) == 0) then "   |" else if ((length str) == 1) then (" " ++ str ++ " |") else (str ++ " |")

      cells :: Int -> [[Char]]
      cells n = (replicate (n1 n) "") ++ [ show d | d <- [1..(monthDays2 n)] ] ++ (replicate (42 - (n1 n) - (monthDays2 n)) "") -- список дней ++ пустые ячейки
                                    where n2 x = 7 - (((monthDays2 x) - (7 - (n1 x))) `mod` 7)
                                          n1 x = (firstDay2 x)-1
      monthLine :: [Char] -> [Char]
      monthLine mnth = "     |" ++ (replicate (round((27 - fromIntegral(length mnth) ) / 2)) ' ') ++ mnth ++  (replicate(27 - n - length mnth ) ' ') ++ "|"
                       where n = round((27 - fromIntegral(length mnth) ) / 2)

      weekLine :: Int -> [Char]
      weekLine n= concat[line2,monthLine (months2 !! n), monthLine (months2 !! (n+1)), monthLine (months2 !! (n+2)),line2,dayLine,dayLine,dayLine,line2]

  putStrLn (calendar)

-- ==============================================================================================================
--                                                  БИОРИТМИК
-- ==============================================================================================================

bio n = do
 if n == 1 then putStrLn "\n              Введите дату рождения и дату рассчета:\n"
 else putStrLn "\n            Попробуй ввести ПРАВИЛЬНО дату рождения и дату рассчета)\n"
 str <- getLine

 let st = splitOn " " str
     x = 1
 if ((length st) /= 6) then bio 2 else return()
 if ((readMaybe (st !! 0) :: Maybe Int) == Nothing) then bio 2 else return()
 if ((readMaybe (st !! 1) :: Maybe Int) == Nothing) then bio 2 else return()
 if ((readMaybe (st !! 2) :: Maybe Int) == Nothing) then bio 2 else return()
 if ((readMaybe (st !! 3) :: Maybe Int) == Nothing) then bio 2 else return()
 if ((readMaybe (st !! 4) :: Maybe Int) == Nothing) then bio 2 else return()
 if ((readMaybe (st !! 5) :: Maybe Int) == Nothing) then bio 2 else return()
 let
     day1 = (read (st !! 0) :: Int)
     month1 = (read (st !! 1) :: Int)
     year1 = (read (st !! 2) :: Integer)
     day2 = (read (st !! 3) :: Int)
     month2 = (read (st !! 4) :: Int)
     year2 = (read (st !! 5) :: Integer)

 if ((checkError1 day1 month1 year1 day2 month2 year2) == 1) then bio 2 else return()

 let
     p = phis day1 month1 year1 day2 month2 year2
     e = emotional day1 month1 year1 day2 month2 year2
     m = mental day1 month1 year1 day2 month2 year2
     p1 = "     Сейчас вы находитесь на переломном моменте фазы.\n     Скорее всего сейчас вы слабы, любая физическая активность требует много усилий.\n     Так что можешь сидеть себе спокойно дома и смотреть сериальчики"
     p2 = "     Сейчас вы находитесь в фазе физического роста.\n     Вы ощущаете прилив энергии и бодрости.\n     Так что давай, сейчас самое время поднимать свою жопу и делать хоть что-нибудь.\n     Cейчас самое подходящее время сесть на диету или начать ходить в зал"
     p3 = "     Сейчас вы находитесь в фазе физического максимума.\n     Энергия прям так и прет, ты сейчас прям как чертов терминатор.\n     Так что давай, направь свою энергию в нужное русло и сделай уже что-нибудь полезное"
     p4 = "     Сейчас вы находитесь в фазе физического спада.\n     Силы и энергия потихоньку покидают в вас.\n     Скорее всего ты сейчас беспомощная и ленивая амебка, но не грусти,\n     зато ты можешь с чистой совестью валяться на диванчике"
     p5 = "     Сейчас вы находитесь в фазе физического минимума.\n     Оооо, сейчас у тебя полное снижение энергичности и выносливости.\n     Так что лучше не рискуй и сиди дома, сейчас самое лучшее время подумать о смысле жизни"
     e1 = "     Сейчас вы находитесь в переломном моменте фазы.\n     Нуууу, сейчас не лучшее время делать что-то серьезное, так что посиди и пережди этот период дома"
     e2 = "     Сейчас вы находитесь в фазе эмоционального подьема.\n     У вас происходит постепенное оживление чувств и прилив бодрости.\n     Так что давай, кофе в рот, пиши код!!!"
     e3 = "     Сейчас Вы находитесь в фазе эмоционального максимума.\n     Ты позитивен как никогда!!! Ты ПОБЕДИТЕЛЬ!!! Тебе по силам свернуть горы, так что используй этот шанс, пока не поздно!!!"
     e4 = "     Сейчас вы находитесь в фазе эмоционального спада.\n     К тебе потихоньку подкрадывается депрессия, страхи и всякие такие неприятные штуки.\n     Любая неприятность, критика может выбить тебя из колеи и вызвать бурную реакцию.\n     Ты сейчас чертов неуравновешенный халк, так что будь осторожнее, береги себя и своих близких"
     e5 = "     Сейчас вы находитесь в фазе эмоционального минимума.\n     Твое состояние граничит с депрессией, апатия, чуство одиночество, раздражительность стали твоими друзьями.\n     Но непереживай, скоро это закончится.\n     Ведь \"Даже после самой темной ночи наступает рассвет, а сильный дождь заканчивается радугой.\" - Николь Рейш"
     m1 = "     Сейчас вы находитесь на переломном моменте фазы.\n     Твой мозг сейчас выполняет перезагрузку, так что не доставай его сложными размышлениями о жизни, а просто отдохни"
     m2 = "     Сейчас вы находитесь в фазе интеллектуального роста.\n     Сейчас отличное время чему-то учиться.\n     Так что давай уже начинай учиться, ты же не хочешь всю жизнь проработать в пятерочке"
     m3 = "     Сейчас вы находитесь в фазе интеллектуального максимума.\n     Ты сейчас прям таки Эйнштейн яву, так что иди, покоряй горы, решай сложные задачи, взламывай Пентагон и всякое такое"
     m4 = "     Сейчас вы находитесь в фазе интеллектуального спада.\n     Ты становишься более рассеянным, все тяжелее делать всякие умные штучки.\n     Будь осторожнее ведь - \"Падение это не провал.Провал - это Провал.Падение - это  где Упал\""
     m5 = "     Сейчас вы находитесь в фазе интеллектуального минимума.\n     Сейчас ты туп как кабачок с грядки, так что на данный момент лучше не усложняй себе жизнь трудными задачами"

 let x = concat ["\n Ваш прогнозик на ",days !! (if (dayInWeek day2 month2 (fromIntegral year2) == 0) then 6 else dayInWeek day2 month2 (fromIntegral year2) - 1), ", ",show day2," ",months !! (month2 - 1)]
 putStrLn x

 putStr "\n     Физическое состояние: "
 putStr(show p)
 putStrLn " %\n"
 if (p <= 5 && p >= -5 ) then putStrLn p1 else if (p > 5 && p <80) then putStrLn p2 else if (p >= 80 && p <=100) then putStrLn p3 else if (p > -80 && p < -5) then putStrLn p4 else putStrLn p5
 putStr "\n     Эмоциональное состояние: "
 putStr(show e)
 putStrLn " %\n"
 if (e <= 5 && e >= -5 ) then putStrLn e1 else if (e > 5 && e <80) then putStrLn e2 else if (e >= 80 && e <=100) then putStrLn e3 else if (e > -80 && e < -5) then putStrLn e4 else putStrLn e5
 putStr "\n     Интеллектуальне состояние: "
 putStr(show m)
 putStrLn " %\n"
 if (m <= 5 && m >= -5 ) then putStrLn m1 else if (m > 5 && m <80) then putStrLn m2 else if (m >= 80 && m <=100) then putStrLn m3 else if (m > -80 && m < -5) then putStrLn m4 else putStrLn m5
