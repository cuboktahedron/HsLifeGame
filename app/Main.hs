module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Debug.Trace
import           System.Random
import           Control.Monad

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

data State = State { cells :: [[Bool]], frame :: Int }

--------------------------
-- シミュレーションの実装
--------------------------
cellWidth :: Float
cellWidth = 5

cellHeight :: Float
cellHeight = 5

fieldSize :: Int
fieldSize = 100

fieldSizeM1 :: Int
fieldSizeM1 = fieldSize - 1

initialCells :: IO State
initialCells = do
  cells <- mapM (\_ -> replicateM fieldSize randomIO) [0 .. fieldSizeM1]
  return (State cells 0)

drawState :: State -> Picture
drawState state =
  let vp = ViewPort { viewPortTranslate = (-320, -320)
                    , viewPortRotate = 0
                    , viewPortScale = 1
                    }
  in applyViewPortToPicture vp
     $ Pictures
     $ drawCells (cells state) ++ drawFrame (frame state)

drawCells :: [[Bool]] -> [Picture]
drawCells cells =
  let pts = [(x, y) | x <- [0 .. fieldSizeM1], y <- [0 .. fieldSizeM1]]
  in map (drawCell cells) pts

drawCell :: [[Bool]] -> (Int, Int) -> Picture
drawCell cells (x, y) =
  let coordX = cellWidth * fromIntegral x
      coordY = cellHeight * fromIntegral y
      cell = cells !! y !! x
  in if cell
     then translate coordX coordY $ rectangleSolid cellWidth cellHeight
     else blank

drawFrame :: Int -> [Picture]
drawFrame frame = [(translate 0 (windowHeight - 40) . scale 0.25 0.25 $ text (show frame))]

nextState :: ViewPort -> Float -> State -> State
nextState vp dt state = State (nextCells (cells state)) ((frame state) + 1)

nextCells :: [[Bool]] -> [[Bool]]
nextCells board =
  let coords = [(x, y) | x <- [0 .. fieldSizeM1], y <- [0 .. fieldSizeM1]]
      cells = map (nextCell board) coords
  in [take fieldSize (drop (fieldSize * i) cells) | i <- [0 .. fieldSizeM1]]

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
          , isActive board (x - 1) (y + 1)
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
                        else board !! x !! y

-------------
-- main 関数
-------------
main :: IO ()
main = do
  cells <- initialCells
  simulate window white 10 cells drawState nextState
