module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Debug.Trace

-------------------
-- Display の設定
-------------------
windowWidth :: Num a => a
windowWidth = 640

windowHeight :: Num a => a
windowHeight = 640

window :: Display
window =
  InWindow "Conway's Game of Life" (windowWidth, windowHeight) (100, 100)

--------------------------
-- シミュレーションの実装
--------------------------
cellWidth :: Float
cellWidth = 50

cellHeight :: Float
cellHeight = 50

fieldSize :: Int
fieldSize = 5

fieldSizeM1 :: Int
fieldSizeM1 = fieldSize - 1

initialCells :: [[Bool]]
initialCells = replicate fieldSize $ replicate fieldSize True

drawCells :: [[Bool]] -> Picture
drawCells cells =
  let pts = [(x, y) | x <- [0 .. fieldSizeM1], y <- [0 .. fieldSizeM1]]
  in Pictures $ map (drawCell cells) pts

drawCell :: [[Bool]] -> (Int, Int) -> Picture
drawCell cells (x, y) =
  let coordX = cellWidth * fromIntegral x
      coordY = cellHeight * fromIntegral y
      cell = cells !! x !! y
  in if cell
     then translate coordX coordY $ rectangleSolid cellWidth cellHeight
     else blank

nextCells :: ViewPort -> Float -> [[Bool]] -> [[Bool]]
nextCells vp dt board =
  let coords = [(x, y) | x <- [0 .. fieldSizeM1], y <- [0 .. fieldSizeM1]]
      cells = map (nextCell board) coords
      b = [take fieldSize (drop (fieldSize * i) cells)
          | i <- [0 .. fieldSizeM1]]
  in b

nextCell :: [[Bool]] -> (Int, Int) -> Bool
nextCell board (x, y) =
  let cell = board !! x !! y
      surround = length
        $ filter
          (== True)
          [ isActive board (x - 1) (y - 1)
          , isActive board x (y - 1)
          , isActive board (x + 1) (y - 1)
          , isActive board (x - 1) y
          , isActive board (x + 1) y
          , isActive board x (y + 1)
          , isActive board (x + 1) (y + 1)]
  in if cell
     then surround == 2 || surround == 3
     else surround == 3

isActive :: [[Bool]] -> Int -> Int -> Bool
isActive board x y = let size = length board
                         outer = x < 0 || x >= size || y < 0 || y >= size
                     in if outer
                        then False
                        else board !! y !! x

-------------
-- main 関数
-------------
main :: IO ()
main = simulate window white 1 initialCells drawCells nextCells
