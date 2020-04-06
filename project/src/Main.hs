{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.Random
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

-- -------------------------- Main -----------------------------

main = do
  putStrLn "\n\n            Приветик)\n            Выбирай что хочешь:\n\n            Month - календарь на месяц\n            Year - календарь на год\n            Bio - если хочешь узнать свой биоритм, тыкай сюда)\n            KN - Крестики-Нолики\n            Exit - выход\n"
  str <- getLine
  if str == "Month" then monthCalen 1
  else if str == "Year" then yearCalen 1
  else if str == "Bio" then bio 1
  else if str == "KN" then calculator
  else if str == "Exit" then putStr "\n            Пока)\n"
  else main

replay n = do
  if n == 1 then putStrLn "\n\n            Хотите чего нибудь еше?)\n            Month - календарь на месяц\n            Year - показать год\n            Bio - если хочешь узнать свой биоритм, тыкай сюда)\n            KN - Крестики-Нолики\n            Exit - выход\n"
  else putStrLn "                Вы ввели что-то непонятное, попробуй еще раз!\n"
  str <- getLine
  if str == "Month" then monthCalen 1
  else if str == "Year" then yearCalen 1
  else if str == "Bio" then bio 1
  else if str == "KN" then calculator
  else if str == "Exit" then putStr "\n            Пока)\n"
  else replay 2

-- -------------------------------------------- МЕСЯЦОК ----------------------------------------

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
 replay 1

--  ------------------------------------------------  ГОД ------------------------------------------------------
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
  replay 1

-- --------------------------------------------------- БИОРИТМИК --------------------------------------------------
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
 replay 1

-- ----------------------------------------- КАЛЬКУЛЯТОР --------------------------------------------------

calculator = do
 x <- rand
 putStrLn "            Выбирай за кого играть: \n            X - играть за 'X' \n            O - играть за 'О'\n            2 - играть за двоих";
 str <- getLine
 if (str == "X" || str == "Х") then do
  startgame "X" 1
 else if (str == "O" || str == "О") then chooseO x
 else if (str == "2") then startgame "*2" 1
 else main
 replay 1

startgame a n = do
 let field = [" "," "," "," "," "," "," "," "," "]
 startshowing
 putStrLn "\n"
 if a == "X" then gameX 5 field
 else if a == "O" then gameO 9 field n
 else firstmove field

chooseO n = do
   if (n == 1) then startgame "O" 1
   else if (n == 2) then startgame "O" 2
   else if (n == 3) then startgame "O" 3
   else if (n == 4) then startgame "O" 4
   else if (n == 5) then startgame "O" 5
   else if (n == 6) then startgame "O" 6
   else if (n == 7) then startgame "O" 7
   else if (n == 8) then startgame "O" 8
   else  startgame "O" 9


p1 field = field!!0
p2 field = field!!1
p3 field = field!!2
p4 field = field!!3
p5 field = field!!4
p6 field = field!!5
p7 field = field!!6
p8 field = field!!7
p9 field = field!!8

showField :: [String] -> IO ()
showField f = putStr ("            +---+---+---+\n            | "++(p1 f)++" | "++(p2 f)++" | "++(p3 f) ++ " |\n            +---+---+---+\n            | "++(p4 f)++" | "++(p5 f)++" | "++(p6 f) ++ " |\n            +---+---+---+\n            | "++(p7 f)++" | "++(p8 f)++" | "++(p9 f) ++ " |\n            +---+---+---+\n\n")


makeField f str = (changef (makemove f "X" "O") (fromEnum( head str) - 48) "O")


startshowing :: IO ()
startshowing = putStr ("Пиши номер ячейки, в которую хочешь ходить) \n            +---+---+---+\n            | 1 | 2 | 3 |\n            +---+---+---+\n            | 4 | 5 | 6 |\n            +---+---+---+\n            | 7 | 8 | 9 |\n            +---+---+---+\n")


victory ::[String] -> String -> Int
victory field a = helper field a
  where
   helper field a | (field !! 0 == a && field !! 1 == a && field !! 2 == a) = 1
                  | (field !! 3 == a && field !! 4 == a && field !! 5 == a) = 1
				  | (field !! 6 == a && field !! 7 == a && field !! 8 == a) = 1
				  | (field !! 0 == a && field !! 3 == a && field !! 6 == a) = 1
                  | (field !! 1 == a && field !! 4 == a && field !! 7 == a) = 1
				  | (field !! 2 == a && field !! 5 == a && field !! 8 == a) = 1
				  | (field !! 0 == a && field !! 4 == a && field !! 8 == a) = 1
				  | (field !! 2 == a && field !! 4 == a && field !! 6 == a) = 1
			      | otherwise = 0

changef:: [a] -> Int -> a -> [a]
changef f n a = (take (n-1) f)++[a]++(reverse (take (9 - n) (reverse f )))

findthree :: Int -> [String] -> String -> Int
findthree 3 f a = -1
findthree n f a = if (f!!(0+n*3) == a && f!!(1+n*3) == a && f!!(2+n*3) == " ") then 2 + n*3
                  else if (f!!(1+n*3) == a && f!!(2+n*3) == a && f!!(0+n*3) == " ") then 0 + n*3
                  else if (f!!(2+n*3) == a && f!!(0+n*3) == a && f!!(1+n*3) == " ") then n*3 + 1
                  else findthree (n+1) f a

findthree2:: Int -> [String] -> String -> Int
findthree2 3 f a = -1
findthree2 n f a = if (f!!(n) == a && f!!(n+3) == a && f!!(6+n) == " ") then 6+n
                   else if (f!!(n) == a && f!!(n+6) == a && f!!(3+n) == " ") then 3+n
                   else if (f!!(n+3) == a && f!!(n+6) == a && f!!(0+n) == " ") then 0+n
                   else findthree2 (n+1) f a

findD f a = if  ( ((f!!0)) == a &&(f!!4) == a && (f!!8) == " ") then 8
            else if (((f!!0)) == a &&((f!!8)) == a && ((f!!4)) == " ") then 4
            else if ((f!!4) == a &&(f!!8) == a && (f!!0) == " ") then 0
            else if ((f!!4) == a &&(f!!2) == a && (f!!6) == " ") then 6
            else if ((f!!4) == a &&(f!!6) == a && (f!!2) == " ") then 2
            else if ((f!!2) == a &&(f!!6) == a && (f!!4) == " ") then 4
            else -1

findnear f a b = helper f a b 1
 where
  helper f a b n = if (f!!n == " " && (f!!(n+1) == b || f!!(n-1) == b)) then n
               else helper f a b (n+1)

makemove f a b | ((f!!4) == " ") = ((take (4) f)++[a]++(reverse (take (4) (reverse f ))))
               | (findthree 0 f a/= -1) = changef f ((findthree 0 f a)+1) a
               | (findthree2 0 f a /= -1) = changef f ((findthree2 0 f a )+1) a
               | (findD f a /= -1) = changef f ((findD f a)+1) a
               | (findthree 0 f b/= -1) = changef f ((findthree 0 f b)+1) a
               | (findthree2 0 f b /= -1) = changef f ((findthree2 0 f b)+1) a
               | (findD f b /= -1) = changef f ((findD f b)+1) a
               | otherwise = changef f ((findnear f a b )+1) a

makemove2 f a b n = ((take (n - 1) f)++[a]++(reverse (take (9 - n) (reverse f ))))

makemove3 f a b n | (n == -2  && f !! 4 == "X") = changef f ((find1 f)+1) "X"
                  | (n == -2 && f !! 4 == "O" && ( any (== "X") [f !! 0,f !! 2,f !! 6,f !! 8])) = changef f ((find2 f)+1) "X"
			      | ((f!!4) == " ") = ((take (4) f)++[a]++(reverse (take (4) (reverse f ))))
                  | (findthree 0 f a/= -1) = changef f ((findthree 0 f a)+1) a
                  | (findthree2 0 f a /= -1) = changef f ((findthree2 0 f a )+1) a
                  | (findD f a /= -1) = changef f ((findD f a)+1) a
                  | (findthree 0 f b/= -1) = changef f ((findthree 0 f b)+1) a
                  | (findthree2 0 f b /= -1) = changef f ((findthree2 0 f b)+1) a
                  | (findD f b /= -1) = changef f ((findD f b)+1) a
				  | (f !! 4 == "O" && (f !! 0 == "X" && f !! 8 == "X" || f !! 2 == "X" && f !! 6 == "X")) = changef f ((find3 f)+1) "X"
				  | ((f !! 0 == "X" && f !! 4 == "X" && f !! 8 == "O") || (f !! 8 == "X" && f !! 4 == "X" && f !! 0 == "O") || (f !! 2 == "X" && f !! 4 == "X" && f !! 6 == "O") || (f !! 6 == "X" && f !! 4 == "X" && f !! 2 == "O" )) = changef f ((find4 f)+1) "X"
                  | otherwise = changef f ((findnear f a b )+1) a


find1 f | (f !! 1 == "O" || f !! 3 == "O" || f !! 5 == "O" || f !! 7 == "O") = 2
        | otherwise = findnear f "X" "O"

find2 f | (f !! 0 == "X" ) = 8
        | (f !! 2 == "X" ) = 6
	    | (f !! 6 == "X" ) = 2
        | (f !! 8 == "X" ) = 0

find3 f | (f !! 0 == "X" && f !! 8 == "X" && f !! 2 == "O") = 6
		| (f !! 0 == "X" && f !! 8 == "X" && f !! 6 == "O") = 2
		| (f !! 2 == "X" && f !! 6 == "X" && f !! 0 == "O") = 8
		| (f !! 2 == "X" && f !! 6 == "X" && f !! 8 == "O") = 0
		| otherwise = findnear f "X" "O"

find4 f | (f !! 0 == "X" && f !! 4 == "X" && f !! 8 == "O") = (if f !! 3 == " " then 3 else if f !! 6 == " " then 6 else findnear f "X" "O")
        | (f !! 8 == "X" && f !! 4 == "X" && f !! 0 == "O") = (if f !! 5 == " " then 5 else if f !! 2 == " " then 2 else findnear f "X" "O")
        | (f !! 2 == "X" && f !! 4 == "X" && f !! 6 == "O") = (if f !! 5 == " " then 5 else if f !! 8 == " " then 8 else findnear f "X" "O")
        | (f !! 6 == "X" && f !! 4 == "X" && f !! 2 == "O") = (if f !! 3 == " " then 3 else if f !! 0 == " " then 0 else findnear f "X" "O")

rand  ::  IO  Int
rand = getStdRandom  (randomR (1, 9))

-- ------------------------------   Игра за Х ------------------------------------------

gameX :: Int -> [String] -> IO ()
gameX  0 f = putStr "     ТАМ ТАМ ТАМ\n     У нас ничья\n"

gameX  n f = do
  putStr "Ваш ход: ";
  str <- getLine;
  putStr "   \n";
  if ( str == [] || tail str /= [] || (f!!(fromEnum( head str) - 48 - 1)) /= " " || head str > '9' || head str <= '0' ) then do
    putStrLn "     Кажется эта ячейка уже занята(((";
    gameX  n f
  else do
    let ch =(fromEnum( head str) - 48)
    let field = changef f ch "X"
    showField field
    if (victory field "X") == 1 then do
      putStrLn "     ГИП ГИП УРАААААААА\n     Вы победили!\n";
    else if n == 1 then gameX  0 f else do{
     putStrLn "Ход бота";
     showField (makemove field  "O"  "X");
     if ( victory (makemove field  "O"  "X") "O" ) > 0 then putStr ("     УУУУУУУ\n     Ты проиграли ((( \n ");
     else gameX  (n-1) (makemove field  "O"  "X")
    }


-- ----------------------------  Игра за Y -----------------------------------------------


gameO 0 f n = putStr "     ТАМ ТАМ ТАМ\n     У нас ничья\n"

gameO n f x = do
  if (x > 0) then do {
  putStrLn "Ход бота";
  showField (makemove2 f  "X"  "O" x);
  gameO (n-1) (makemove2 f "X"  "O" x) 0
  }
  else if (n `mod` 2 == 1) then do {
   putStrLn "Ход бота";
   showField (makemove3 f "X" "O" x);
   if ( victory (makemove3 f "X" "O" x) "X" ) == 1 then putStr ("     УУУУУУУ\n     Ты проиграли ((( \n ");
   else gameO (n-1) (makemove3 f "X" "O" x) 0
  }
  else do {
   putStr "Ваш ход: ";
   str <- getLine;
   putStr "   \n";
   if ( str == [] || tail str /= [] || (f!!(fromEnum( head str) - 48 - 1)) /= " " || head str > '9' || head str <= '0' ) then do
    putStrLn "     Кажется эта ячейка уже занята(((";
    gameO  n f 0
   else do
    let ch =(fromEnum( head str) - 48)
    let field = changef f ch "O"
    showField field
    if (victory field "O") == 1 then do
      putStrLn "     ГИП ГИП УРАААААААА\n     Вы победили!\n";
    else do
	 if (n == 8 ) then gameO (n-1) field (-2)
	 else gameO (n-1) field (0)
  }

-- ------------------------------- Игра на двоих --------------------------------

firstmove:: [String] -> IO ()
firstmove f = do
 if (victory f "O" >= 1) then putStrLn "Победил второй игрок"
 else do
  putStr "Ход первого игрока: "
  str <- getLine
  if ( str /= [] && tail str == [] && (f!!(fromEnum( head str) - 48 - 1)) == " " && head str <= '9' && head str >= '0' ) then do
   showField (changef f (fromEnum( head str) - 48) "X" )
   move2 (changef f (fromEnum( head str) - 48) "X" )
  else firstmove f

move2:: [String] -> IO ()
move2 f = do
 if any (" "==) f then do{
  if (victory f "X" >= 1) then putStrLn "     Победил первый игрок\n";
  else do
   putStr "     Ход второго игрока: \n";
   str <- getLine
   if ( str /= [] && tail str == [] && (f!!(fromEnum( head str) - 48 - 1)) == " " && head str <= '9' && head str >= '0' ) then do
    showField (changef f (fromEnum( head str) - 48) "O" )
    firstmove (changef f (fromEnum( head str) - 48) "O" )
   else move2 f
 }
 else putStrLn "     ТАМ ТАМ ТАМ\n     У нас ничья\n";

