{-# LANGUAGE OverloadedStrings #-}
module Main where

import Krestiki_noliki
import Calendar
-- -------------------------- Main -----------------------------

main = do
  putStrLn "\n\n            Приветик)\n            Выбирай что хочешь:\n\n            Month - календарь на месяц\n            Year - календарь на год\n            Bio - если хочешь узнать свой биоритм, тыкай сюда)\n            KN - Крестики-Нолики\n            Exit - выход\n"
  str <- getLine
  if str == "Month" then do
    monthCalen 1
    replay 1
  else if str == "Year" then do
    yearCalen 1
    replay 1
  else if str == "Bio" then do
    bio 1
    replay 1
  else if str == "KN" then do
    calculator
    replay 1
  else if str == "Exit" then putStr "\n            Пока)\n"
  else main

replay n = do
  if n == 1 then putStrLn "\n\n            Хотите чего нибудь еше?)\n            Month - календарь на месяц\n            Year - показать год\n            Bio - если хочешь узнать свой биоритм, тыкай сюда)\n            KN - Крестики-Нолики\n            Exit - выход\n"
  else putStrLn "                Вы ввели что-то непонятное, попробуй еще раз!\n"
  str <- getLine
  if str == "Month" then do
     monthCalen 1
     replay 1
   else if str == "Year" then do
     yearCalen 1
     replay 1
   else if str == "Bio" then do
     bio 1
     replay 1
   else if str == "KN" then do
     calculator
     replay 1
   else if str == "Exit" then putStr "\n            Пока)\n"
   else replay 2





