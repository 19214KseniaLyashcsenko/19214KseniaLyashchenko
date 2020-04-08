{-# LANGUAGE OverloadedStrings #-}
module Krestiki_noliki where
import System.Random
--import Main

-- ==============================================================================================================
--                                                  КРЕСТИКИ-НОЛИКИ
-- ==============================================================================================================

calculator = do
 x <- rand
 putStrLn "            Выбирай за кого играть: \n            X - играть за 'X' \n            O - играть за 'О'\n            2 - играть на двоих";
 str <- getLine
 if (str == "X") then do
  startgame "X" x
 else if (str == "O") then startgame "O" x
 else if (str == "2") then startgame "2" x
 else calculator

startgame a n = do
 let field = [" "," "," "," "," "," "," "," "," "]
 start
 putStrLn "\n"
 if a == "X" then gameX 5 field n
 else if a == "O" then gameO 9 field n
 else game1 field


p1 field = field!!0
p2 field = field!!1
p3 field = field!!2
p4 field = field!!3
p5 field = field!!4
p6 field = field!!5
p7 field = field!!6
p8 field = field!!7
p9 field = field!!8


start = putStr ("Пиши номер ячейки, в которую хочешь ходить) \n            +---+---+---+\n            | 1 | 2 | 3 |\n            +---+---+---+\n            | 4 | 5 | 6 |\n            +---+---+---+\n            | 7 | 8 | 9 |\n            +---+---+---+\n")

showField f = putStr ("            +---+---+---+\n            | "++(p1 f)++" | "++(p2 f)++" | "++(p3 f) ++ " |\n            +---+---+---+\n            | "++(p4 f)++" | "++(p5 f)++" | "++(p6 f) ++ " |\n            +---+---+---+\n            | "++(p7 f)++" | "++(p8 f)++" | "++(p9 f) ++ " |\n            +---+---+---+\n\n")

changeField f n a = (take (n-1) f)++[a]++(reverse (take (9 - n) (reverse f )))


makemove f a b n x | (find5 0 f a/= -1) = changeField f ((find5 0 f a)+1) a
                   | (find6 0 f a /= -1) = changeField f ((find6 0 f a )+1) a
                   | (findD f a /= -1) = changeField f ((findD f a)+1) a
                   | (find5 0 f b/= -1) = changeField f ((find5 0 f b)+1) a
                   | (find6 0 f b /= -1) = changeField f ((find6 0 f b)+1) a
                   | (findD f b /= -1) = changeField f ((findD f b)+1) a
				           | ((f!!4) == " ") = ((take (4) f)++[a]++(reverse (take (4) (reverse f ))))
                   | otherwise = changeField f ((findnear f a b )+1) a

makemove2 f a b n = ((take (n - 1) f)++[a]++(reverse (take (9 - n) (reverse f ))))

makemove3 f a b n | (n == -2  && f !! 4 == "X") = changeField f ((find1 f)+1) "X"
                  | (n == -2 && f !! 4 == "O" && ( any (== "X") [f !! 0,f !! 2,f !! 6,f !! 8])) = changeField f ((find2 f)+1) "X"
			      | ((f!!4) == " ") = ((take (4) f)++[a]++(reverse (take (4) (reverse f ))))
                  | (find5 0 f a/= -1) = changeField f ((find5 0 f a)+1) a
                  | (find6 0 f a /= -1) = changeField f ((find6 0 f a )+1) a
                  | (findD f a /= -1) = changeField f ((findD f a)+1) a
                  | (find5 0 f b/= -1) = changeField f ((find5 0 f b)+1) a
                  | (find6 0 f b /= -1) = changeField f ((find6 0 f b)+1) a
                  | (findD f b /= -1) = changeField f ((findD f b)+1) a
				  | (f !! 4 == "O" && (f !! 0 == "X" && f !! 8 == "X" || f !! 2 == "X" && f !! 6 == "X")) = changeField f ((find3 f)+1) "X"
				  | ((f !! 0 == "X" && f !! 4 == "X" && f !! 8 == "O") || (f !! 8 == "X" && f !! 4 == "X" && f !! 0 == "O") || (f !! 2 == "X" && f !! 4 == "X" && f !! 6 == "O") || (f !! 6 == "X" && f !! 4 == "X" && f !! 2 == "O" )) = changeField f ((find4 f)+1) "X"
                  | otherwise = changeField f ((findnear f a b )+1) a


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

find5 :: Int -> [String] -> String -> Int
find5 3 f a = -1
find5 n f a | (f!!(0+n*3) == a && f!!(1+n*3) == a && f!!(2+n*3) == " ") = 2 + n*3
            | (f!!(1+n*3) == a && f!!(2+n*3) == a && f!!(0+n*3) == " ") = 0 + n*3
            | (f!!(2+n*3) == a && f!!(0+n*3) == a && f!!(1+n*3) == " ") = n*3 + 1
            | otherwise = find5 (n+1) f a

find6:: Int -> [String] -> String -> Int
find6 3 f a = -1
find6 n f a | (f!!(n) == a && f!!(n+3) == a && f!!(6+n) == " ") = 6+n
            | (f!!(n) == a && f!!(n+6) == a && f!!(3+n) == " ") = 3+n
            | (f!!(n+3) == a && f!!(n+6) == a && f!!(0+n) == " ") = 0+n
            | otherwise = find6 (n+1) f a

findD f a | ( ((f!!0)) == a &&(f!!4) == a && (f!!8) == " ") = 8
          |(((f!!0)) == a &&((f!!8)) == a && ((f!!4)) == " ") = 4
          | ((f!!4) == a &&(f!!8) == a && (f!!0) == " ") = 0
          | ((f!!4) == a &&(f!!2) == a && (f!!6) == " ") = 6
          | ((f!!4) == a &&(f!!6) == a && (f!!2) == " ") = 2
          | ((f!!2) == a &&(f!!6) == a && (f!!4) == " ") = 4
          | otherwise = -1

findnear f a b = helper f a b 1
 where
  helper f a b n = if (f!!n == " " && (f!!(n+1) == b || f!!(n-1) == b)) then n
               else helper f a b (n+1)

victory f a n = if ((any (== 1) (line n f a n)) == True || (any (== 1) (column n f a 0)) == True || (diagonal n f a) == 1) then 1 else 0




line n f a 0 = []
line n f a k = [(foldl (\x y -> if (y /= a) then x - 1 else x + 0) 1 (take n f))] ++ line n (drop n f ) a (k-1)

column n f a 3 = []
column n f a k =  [(foldl (\x y -> if (y /= a) then x - 1 else x + 0) 1 (list n f k n))] ++ column n f a (k + 1)
  where list n f k 0 = []
        list n f k x = [f !! (k + n * (x - 1))] ++ list n f k (x-1)

diagonal n f a = if (f !! 0 == a && f !! 4 == a && f !! 8 == a || f !! 2 == a && f !! 4 == a && f !! 6 == a) then 1 else 0

rand  ::  IO  Int
rand = getStdRandom  (randomR (1, 9))

-- ==============================================================================================================
--                                                  ИГРА ЗА Х
-- ==============================================================================================================

gameX  0 f x = putStr "     ТАМ ТАМ ТАМ\n     У нас ничья\n"

gameX  n f x = do
  putStr "Ваш ход: ";
  str <- getLine;
  putStr "   \n";
  if ( str == [] || tail str /= []  || head str > '9' || head str <= '0' ) then do
    putStrLn "     Вы ввели что-то непонятно неясное((("
    gameX  n f x
  else if (f!!(fromEnum( head str) - 48 - 1)) /= " " then do
    putStrLn "     Кажется эта ячейка уже занята((("
    gameX  n f x
  else do
    let ch =(fromEnum( head str) - 48)
    let field = changeField f ch "X"
    showField field
    if (victory field "X" 3) == 1 then do
      putStrLn "     ГИП ГИП УРАААААААА\n     Вы победили!\n"
    else if n == 1 then gameX  0 f x else do{
     putStrLn "Ход бота";
     if (n == 5 && x /= ch) then do
      showField (changeField field x "O")
      gameX  (n-1) (changeField field x "O") x
     else do
      showField (makemove field  "O"  "X" n x);
      if ( victory (makemove field  "O"  "X" n x) "O" 3) > 0 then putStr ("     УУУУУУУ\n     Ты проиграл((( \n ")
      else gameX  (n-1) (makemove field  "O"  "X" n x) x
    }


-- ==============================================================================================================
--                                                  ИГРА ЗА Y
-- ==============================================================================================================


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
   if ( victory (makemove3 f "X" "O" x) "X" 3) == 1 then putStr ("     УУУУУУУ\n     Ты проиграл((( \n ");
   else gameO (n-1) (makemove3 f "X" "O" x) 0
  }
  else do {
   putStr "Ваш ход: ";
   str <- getLine;
   putStr "   \n";
   if ( str == [] || tail str /= [] ||  head str > '9' || head str <= '0' ) then do
     putStrLn "     Вы ввели что-то непонятно неясное(((";
     gameO  n f 0
   else if ((f!!(fromEnum( head str) - 48 - 1)) /= " ") then do
     putStrLn "     Кажется эта ячейка уже занята(((";
     gameO  n f 0
   else do
    let ch =(fromEnum( head str) - 48)
    let field = changeField f ch "O"
    showField field
    if (victory field "O" 3) == 1 then do
      putStrLn "     ГИП ГИП УРАААААААА\n     Вы победили!\n";
    else do
	 if (n == 8 ) then gameO (n-1) field (-2)
	 else gameO (n-1) field (0)
  }

-- ==============================================================================================================
--                                                  ИГРА НА ДВОИХ
-- ==============================================================================================================

game1:: [String] -> IO ()
game1 f = do
 if (victory f "O" 3 >= 1) then putStrLn "Победил второй игрок"
 else do
  putStr "Ход первого игрока: \n"
  str <- getLine
  if ( str == [] || tail str /= [] ||  head str > '9' || head str <= '0' ) then do
   putStrLn "     Вы ввели что-то непонятно неясное(((";
   game1 f
  else if (f!!(fromEnum( head str) - 48 - 1)) /= " " then do
   putStrLn "     Кажется эта ячейка уже занята(((";
   game1 f
  else do
     showField (changeField f (fromEnum( head str) - 48) "X" )
     game2 (changeField f (fromEnum( head str) - 48) "X" )

game2:: [String] -> IO ()
game2 f = do
 if any (" "==) f then do{
  if (victory f "X" 3 >= 1) then putStrLn "     Победил первый игрок\n";
  else do
   putStr "     Ход второго игрока: \n";
   str <- getLine
   if  ( str == [] || tail str /= [] ||  head str > '9' || head str <= '0' ) then do
    putStrLn "     Вы ввели что-то непонятно неясное(((";
    game2 f
   else if (f!!(fromEnum( head str) - 48 - 1)) /= " " then do
    putStrLn "     Кажется эта ячейка уже занята(((";
    game2 f
   else do
        showField (changeField f (fromEnum( head str) - 48) "O" )
        game1 (changeField f (fromEnum( head str) - 48) "O" )
 }
 else putStrLn "     ТАМ ТАМ ТАМ\n     У нас ничья\n";
  


   
   