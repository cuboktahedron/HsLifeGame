module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Game
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

data State = State { cells :: [[Bool]]
                   , frame :: Int
                   , isPaused :: Bool
                   , isOneTime :: Bool
                   }

defState :: State
defState =
  State { cells = [[]], frame = 0, isPaused = False, isOneTime = False }

--------------------------
-- シミュレーションの実装
--------------------------
fieldSize :: Int
fieldSize = 30

cellWidth :: Float
cellWidth = (500 :: Float) / (fromIntegral fieldSize)

cellHeight :: Float
cellHeight = (500 :: Float) / (fromIntegral fieldSize)

fieldSizeM1 :: Int
fieldSizeM1 = fieldSize - 1

initialCells :: IO State
initialCells = do
  cells <- mapM (\_ -> replicateM fieldSize randomIO) [0 .. fieldSizeM1]
  return defState { cells = cells, isPaused = True }

drawState :: State -> Picture
drawState state =
  let vp = ViewPort { viewPortTranslate =
                        ((-320 + cellWidth), (-320 + cellHeight))
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
drawFrame frame =
  [ (translate 0 (windowHeight - (cellHeight + 40)) . scale 0.25 0.25
     $ text (show frame))]

nextState :: Float -> State -> State
nextState dt state
  | (isOneTime state) = state { cells = nextCells (cells state)
                              , frame = ((frame state) + 1)
                              , isPaused = True
                              , isOneTime = False
                              }
  | (isPaused state) = state
  | otherwise =
    state { cells = nextCells (cells state), frame = ((frame state) + 1) }

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

operate :: (Event -> State -> State)
operate (EventKey key ks _ _) state = operateWithKey key ks state
operate (EventMotion _) state = state
operate (EventResize _) state = state

operateWithKey :: Key -> KeyState -> State -> State
operateWithKey (MouseButton LeftButton) ks state =
  if ks == Down
  then state { isOneTime = True }
  else state
operateWithKey (Char 's') ks state =
  if ks == Down
  then state { isPaused = not (isPaused state) }
  else state
operateWithKey _ _ state = state

-------------
-- main 関数
-------------
main :: IO ()
main = do
  cells <- initialCells
  play window white 5 cells drawState operate nextState
