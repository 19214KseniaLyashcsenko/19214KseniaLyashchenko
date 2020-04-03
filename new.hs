{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.Functor
import Data.List
import Control.Monad
import Data.Text (unpack, pack)
import qualified Data.Text as T
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Data.IORef



year d1 m1 y1 d2 m2 y2 | (m2 > m1) = y2 - y1
                       | (m2 == m1 && d2 > d1 ) = y2 - y1
                       | otherwise = y2 - y1 - 1

time d1 m1 y1 d2 m2 y2 = fromIntegral(year d1 m1 y1 d2 m2 y2  * 365 + vesYear y1 (y2-1) 0 1 + days d1 m1 d2 (m2-1) y2 d2)
  where vesYear y1 y2 k 0 = k
        vesYear y1 y2 k f = vesYear (y1 + 1) y2 (if y1 `mod` 4 == 0 then k + 1 else k) (if y1 == y2 then f - 1 else f)
        days d1 m1 d2 0 y2 k = k
        days d1 m1 d2 m2 y2 k = days d1 m1 d2 (if (m2 == m1) then m2 - m2 else m2 - 1) y2 (if (m2 == m1) then k + kDays m2 y2 0 - d1 else k + kDays m2 y2 0)
          where kDays m y k | (m == 4 || m == 6 || m == 10 || m == 12) = k + 30
                            | (m == 2 && y `mod` 4 == 0) = k + 29
                            | (m == 2) = k + 28
                            | otherwise = k + 31

phis d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 23 )* 100 )

emotional d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 28 )* 100 )

mental d1 m1 y1 d2 m2 y2 = if (d1 == d2) && (m1 == m2) && (y1 == y2) then 0 else Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 33 )* 100 )

day d m y = (i_y + i_m + d + (if (y `div` 1000 == 2) then  0 else 1) + (if (y `mod` 4 == 0 && m == 1 || y `mod` 4 == 0 && m == 2)then -1 else 0)) `mod` 7
 where i_m | (m == 1) || (m == 10) = 6
           | (m == 2) || (m == 3) || (m == 11) = 2
           | (m == 4) || (m == 7) = 5
           | (m == 5) = 0
           | (m == 6) = 3
           | (m == 8) = 1
           | (m == 9) || (m == 12) = 4
       i_y = (x `div` 12) + (x `mod` 12) + ((x `mod` 12) `div` 4)
         where x = y `mod` 100


showResults :: [Ref TextBuffer] -> [Ref TextBuffer] -> Ref Button -> IO ()
showResults buffers resultBuffers _ = do
  textList <- mapM getText buffers -- [Число, месяц, год, число, месяц, год]
  let (d1:m1:y1:d2:m2:y2:_) = map (read . unpack) textList
  let (buff1:buff2:buff3:buff4:_) = resultBuffers
  setText buff1 (pack $ show $ phis d1 m1 y1 d2 m2 y2)
  setText buff2 (pack $ show $ emotional d1 m1 y1 d2 m2 y2)
  setText buff3 (pack $ show $ mental d1 m1 y1 d2 m2 y2)
  setText buff4 (pack $ show $ if day d2 m2 y2 == 0 then "Sunday" else if day d2 m2 y2 == 1 then "Monday"
  else if day d2 m2 y2 == 2 then "Tuesday" else if day d2 m2 y2 == 3 then "Wednesday" else if day d2 m2 y2 == 4 then "Thursday" else if day d2 m2 y2 == 5 then "Friday" else "Saturday")
  return ()



bt1 :: IORef Int -> T.Text -> Boxtype -> Bool -> IO ()
bt1 n1 label' boxtype' square' = do
  b1' <- boxNewWithBoxtype
          boxtype'
          (Rectangle (Position (X (275)) (Y (50)))(Size (Width $ 100) (Height $ 40)))
          label'
  setLabelsize b1' (FontSize 16)

bt2 :: IORef Int -> T.Text -> Boxtype -> Bool -> IO ()
bt2 n2 label' boxtype' square' = do
  b2' <- boxNewWithBoxtype
          boxtype'
          (Rectangle (Position (X (275)) (Y (155)))(Size (Width $ 100) (Height $ 40)))
          label'
  setLabelsize b2' (FontSize 16)

bt3 :: IORef Int -> T.Text -> Boxtype -> Bool -> IO ()
bt3 n3 label' boxtype' square' = do
  b3' <- boxNewWithBoxtype
          boxtype'
          (Rectangle (Position (X (250)) (Y (300)))(Size (Width $ 100) (Height $ 40)))
          label'
  setLabelsize b3' (FontSize 16)

bt4 :: IORef Int -> T.Text -> Boxtype -> Bool -> IO ()
bt4 n4 label' boxtype' square' = do
  b4' <- boxNewWithBoxtype
          boxtype'
          (Rectangle (Position (X (233)) (Y (330)))(Size (Width $ 100) (Height $ 40)))
          label'
  setLabelsize b4' (FontSize 16)

bt5 :: IORef Int -> T.Text -> Boxtype -> Bool -> IO ()
bt5 n5 label' boxtype' square' = do
  b5' <- boxNewWithBoxtype
          boxtype'
          (Rectangle (Position (X (223)) (Y (360)))(Size (Width $ 100) (Height $ 40)))
          label'
  setLabelsize b5' (FontSize 16)



main :: IO ()
main = do


  window <- doubleWindowNew (toSize (640,480)) Nothing (Just "My Cool Project")
  setBox window FlatBox
  FL.getSystemColors
  setColor window (Color 254)
  n1' <- newIORef 0
  n2' <- newIORef 0
  n3' <- newIORef 0
  n4' <- newIORef 0
  n5' <- newIORef 0
  begin window
  bt1 n1' "Приветик!\n Если хочешь рассчитать биоритм,\n То смелее вводи дату рождения" NoBox False
  bt2 n2' "А теперь дату денечка на который рассчитать" NoBox False
  bt3 n3' "Физическое состояние:" NoBox False
  bt4 n4' "Эмоциональное состояние:" NoBox False
  bt5 n5' "Интеллектуальное состояние:" NoBox False



  buff1 <- textBufferNew Nothing Nothing
  edit1 <- textEditorNew (toRectangle (280,120,(30-4),(30-4))) Nothing
  setBuffer edit1 (Just buff1)

  buff2 <- textBufferNew Nothing Nothing
  edit2 <- textEditorNew (toRectangle (310,120,(30-4),(30-4))) Nothing
  setBuffer edit2 (Just buff2)

  buff3 <- textBufferNew Nothing Nothing
  edit3 <- textEditorNew (toRectangle (340,120,(45-4),(30-4))) Nothing
  setBuffer edit3 (Just buff3)

  buff4 <- textBufferNew Nothing Nothing
  edit4 <- textEditorNew (toRectangle (280,210,(30-4),(30-4))) Nothing
  setBuffer edit4 (Just buff4)

  buff5 <- textBufferNew Nothing Nothing
  edit5 <- textEditorNew (toRectangle (310,210,(30-4),(30-4))) Nothing
  setBuffer edit5 (Just buff5)

  buff6 <- textBufferNew Nothing Nothing
  edit6 <- textEditorNew (toRectangle (340,210,(45-4),(30-4))) Nothing
  setBuffer edit6 (Just buff6)

  butt <- buttonNew (toRectangle (315, 260, 40, 25))  (Just "Жмяк")

  resultBuff1 <- textBufferNew Nothing Nothing
  show1 <- textEditorNew (toRectangle (400,310,(160-4),(30-4))) Nothing
  setText resultBuff1 "line"
  setColor show1 (Color 254)
  setBuffer show1 (setText resultBuff1 "line" Just resultBuff1)

  resultBuff2 <- textBufferNew Nothing Nothing
  show2 <- textEditorNew (toRectangle (400,340,(60-4),(30-4))) Nothing
  setColor show2 (Color 254)
  setBuffer show2 (Just resultBuff2)

  resultBuff3 <- textBufferNew Nothing Nothing
  show3 <- textEditorNew (toRectangle (400,370,(60-4),(30-4))) Nothing
  setColor show3 (Color 254)
  setBuffer show3 (Just resultBuff3)

  resultBuff4 <- textBufferNew Nothing Nothing
  show4 <- textEditorNew (toRectangle (400,400,(80-4),(30-4))) Nothing
  setColor show4 (Color 254)
  setBuffer show4 (Just resultBuff4)

  buff3 <- textBufferNew Nothing Nothing
  edit3 <- textEditorNew (toRectangle (340,120,(45-4),(30-4))) Nothing
  setBuffer edit3 (Just buff3)
  setCallback butt  (showResults [buff1, buff2, buff3, buff4, buff5, buff6] [resultBuff1, resultBuff2, resultBuff3, resultBuff4])
  setResizable window (Just window)
  end window
  showWidget window
  _ <- FL.run
  return ()
