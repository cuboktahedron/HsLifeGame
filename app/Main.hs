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
                   , pt :: (Int, Int)
                   }

defState :: State
defState = State { cells = [[]]
                 , frame = 0
                 , isPaused = False
                 , isOneTime = False
                 , pt = (-1, -1)
                 }

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
initialCells =
  let cells = map (\_ -> replicate fieldSize False) [0 .. fieldSizeM1]
  in return defState { cells = cells, isPaused = True }

randomCells :: IO State
randomCells = do
  cells <- mapM (\_ -> replicateM fieldSize randomIO) [0 .. fieldSizeM1]
  return defState { cells = cells, isPaused = True }

drawStateIO :: State -> IO Picture
drawStateIO state = return $ drawState state

drawState :: State -> Picture
drawState state =
  let vp = ViewPort { viewPortTranslate =
                        ((-320 + cellWidth), (-320 + cellHeight))
                    , viewPortRotate = 0
                    , viewPortScale = 1
                    }
  in applyViewPortToPicture vp
     $ Pictures
     $ drawCells (cells state)
     ++ [drawCursor (pt state)]
     ++ drawFrame (frame state)

drawCursor :: (Int, Int) -> Picture
drawCursor (x, y) =
  if isOuter (x, y)
  then blank
  else translate (fromIntegral (x) * cellWidth) (fromIntegral (y) * cellHeight)
    $ color red
    $ rectangleSolid cellWidth cellHeight

isOuter :: (Int, Int) -> Bool
isOuter (x, y) = (x < 0 || x >= fieldSize || y < 0 || y >= fieldSize)

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

nextStateIO :: Float -> State -> IO State
nextStateIO dt state = return $ nextState dt state

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
nextCells cells =
  let coords = [(x, y) | x <- [0 .. fieldSizeM1], y <- [0 .. fieldSizeM1]]
      lineCells = map (nextCell cells) coords
  in [take fieldSize (drop (fieldSize * i) lineCells)
     | i <- [0 .. fieldSizeM1]]

nextCell :: [[Bool]] -> (Int, Int) -> Bool
nextCell cells (x, y) =
  let cell = cells !! x !! y
      surround = length
        $ filter
          (== True)
          [ isActive cells (x - 1) (y - 1)
          , isActive cells x (y - 1)
          , isActive cells (x + 1) (y - 1)
          , isActive cells (x - 1) y
          , isActive cells (x + 1) y
          , isActive cells (x - 1) (y + 1)
          , isActive cells x (y + 1)
          , isActive cells (x + 1) (y + 1)]
  in if cell
     then surround == 2 || surround == 3
     else surround == 3

isActive :: [[Bool]] -> Int -> Int -> Bool
isActive board x y =
  let size = length board
      xx = case x of
        _
          | x < 0 -> size - 1
          | x == size -> 0
          | otherwise -> x
      yy = case y of
        _
          | y < 0 -> size - 1
          | y == size -> 0
          | otherwise -> y
  in board !! xx !! yy

operateIO :: Event -> State -> IO State
operateIO (EventKey key ks _ _) state = operateWithKeyIO key ks state
operateIO (EventMotion pt) state = operateMotionIO pt state
operateIO (EventResize _) state = return state

operateWithKeyIO :: Key -> KeyState -> State -> IO State
operateWithKeyIO (MouseButton LeftButton) ks state = return
  $ if ks == Down
    then state { isOneTime = True }
    else state
operateWithKeyIO (MouseButton RightButton) ks state = return
  $ if ks == Down
    then state { cells = (turnCell (pt state) (cells state))
               , isPaused = True
               , frame = 0
               }
    else state
operateWithKeyIO (Char 'c') ks state =
  if ks == Down
  then initialCells
  else return state
operateWithKeyIO (Char 'r') ks state =
  if ks == Down
  then randomCells
  else return state
operateWithKeyIO (Char 's') ks state = return
  $ if ks == Down
    then state { isPaused = not (isPaused state) }
    else state
operateWithKeyIO _ _ state = return state

turnCell :: (Int, Int) -> [[Bool]] -> [[Bool]]
turnCell (x, y) cells =
  if isOuter (x, y)
  then cells
  else [if y == yy
        then turnCell' (cells !! yy)
        else cells !! yy
       | yy <- [0 .. fieldSizeM1]]
  where
    turnCell' cs = [if x == xx
                    then not (cs !! xx)
                    else cs !! xx
                   | xx <- [0 .. fieldSizeM1]]

operateMotionIO :: (Float, Float) -> State -> IO State
operateMotionIO (ptx, pty) state =
  let x = round ((ptx + 320 - cellWidth) / cellWidth)
      y = round ((pty + 320 - cellHeight) / cellHeight)
  in return state { pt = (x, y) }

-------------
-- main 関数
-------------
main :: IO ()
main = do
  cells <- initialCells
  playIO window white 5 cells drawStateIO operateIO nextStateIO
