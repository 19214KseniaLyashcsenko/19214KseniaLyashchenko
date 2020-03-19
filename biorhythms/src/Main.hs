{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.Functor
import Data.List
import Data.Text (unpack, pack)
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

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

phis d1 m1 y1 d2 m2 y2 = Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 23 )* 100 )

emotional d1 m1 y1 d2 m2 y2 = Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 28 )* 100 )

mental d1 m1 y1 d2 m2 y2 = Prelude.round (sin (2 * pi * (time d1 m1 y1 d2 m2 y2) / 33 )* 100 )

showResults :: [Ref TextBuffer] -> [Ref TextBuffer] -> Ref Button -> IO ()
showResults buffers resultBuffers _ = do
  textList <- mapM getText buffers -- [Число, месяц, год, число, месяц, год]
  let (d1:m1:y1:d2:m2:y2:_) = map (read . unpack) textList
  let (buff1:buff2:buff3:_) = resultBuffers
  setText buff1 (pack $ show $ phis d1 m1 y1 d2 m2 y2)
  setText buff2 (pack $ show $ emotional d1 m1 y1 d2 m2 y2)
  setText buff3 (pack $ show $ mental d1 m1 y1 d2 m2 y2)
  return ()

main :: IO ()
main = do
  win <- doubleWindowNew (toSize (640,480)) Nothing (Just "Simple Fl_Text_Editor")
  setBox win FlatBox
  FL.getSystemColors
  setColor win (Color 254)

  buff1 <- textBufferNew Nothing Nothing
  edit1 <- textEditorNew (toRectangle (210,20,(30-4),(30-4))) Nothing
  setBuffer edit1 (Just buff1)

  buff2 <- textBufferNew Nothing Nothing
  edit2 <- textEditorNew (toRectangle (240,20,(30-4),(30-4))) Nothing
  setBuffer edit2 (Just buff2)

  buff3 <- textBufferNew Nothing Nothing
  edit3 <- textEditorNew (toRectangle (270,20,(45-4),(30-4))) Nothing
  setBuffer edit3 (Just buff3)

  buff4 <- textBufferNew Nothing Nothing
  edit4 <- textEditorNew (toRectangle (345,20,(30-4),(30-4))) Nothing
  setBuffer edit4 (Just buff4)

  buff5 <- textBufferNew Nothing Nothing
  edit5 <- textEditorNew (toRectangle (375,20,(30-4),(30-4))) Nothing
  setBuffer edit5 (Just buff5)

  buff6 <- textBufferNew Nothing Nothing
  edit6 <- textEditorNew (toRectangle (405,20,(45-4),(30-4))) Nothing
  setBuffer edit6 (Just buff6)

  butt <- buttonNew (toRectangle (315, 70, 25, 25))  (Just "OK")

  resultBuff1 <- textBufferNew Nothing Nothing
  show1 <- textEditorNew (toRectangle (240,120,(60-4),(30-4))) Nothing
  setBuffer show1 (Just resultBuff1)

  resultBuff2 <- textBufferNew Nothing Nothing
  show2 <- textEditorNew (toRectangle (300,120,(60-4),(30-4))) Nothing
  setBuffer show2 (Just resultBuff2)

  resultBuff3 <- textBufferNew Nothing Nothing
  show3 <- textEditorNew (toRectangle (360,120,(60-4),(30-4))) Nothing
  setBuffer show3 (Just resultBuff3)

  buff3 <- textBufferNew Nothing Nothing
  edit3 <- textEditorNew (toRectangle (270,20,(45-4),(30-4))) Nothing
  setBuffer edit3 (Just buff3)
  setCallback butt  (showResults [buff1, buff2, buff3, buff4, buff5, buff6] [resultBuff1, resultBuff2, resultBuff3])
  showWidget win
  _ <- FL.run
  return ()

