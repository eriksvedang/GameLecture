module Shnake where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import Control.Monad.State

window = InWindow "Shnake" (400, 400) (10, 10)

-- | width and height of a tile, in pixels
tileSize :: Float
tileSize = 20

-- | nr of tiles in each direction
boardSize :: Int
boardSize = 18

-- | time before the snake moves again
stepTime :: Float
stepTime = 0.1

shnake :: IO ()
shnake = do g <- newStdGen
            play window white 100 (makeWorld g) draw onEvent tick

type Pos = (Int, Int)

data Dir = N | E | S | W deriving (Show, Eq)

data World = World {
    randg :: StdGen,   
    snake :: Snake,
    cherry :: Maybe Pos,
    time :: Float
} deriving Show

data Snake = Snake {
    body :: [Pos],
    dir :: Dir,
    moveTimer :: Float,
    alive :: Bool
} deriving Show

makeWorld :: StdGen -> World
makeWorld g = World {
    randg = g,
    snake = makeSnake,
    cherry = Nothing,
    time = 0.0
}

makeSnake :: Snake
makeSnake = Snake { 
    body = [(x,10) | x <- [15..17]],
    dir = W,
    moveTimer = 0.0,
    alive = True
}

-- Drawing

draw :: World -> Picture
draw world = applyViewPortToPicture viewPort $ pictures [cherryPic, snakePic]
    where cherryPic = drawCherry (cherry world) worldTime
          snakePic  = drawSnake (snake world)
          worldTime = time world

viewPort = ViewPort (-180, -180) 0 1.0

drawSnake :: Snake -> Picture
drawSnake s = color c $ pictures $ map drawSegment $ body s
    where c = if alive s then black else makeColor 0.8 0.8 0.7 1.0

drawSegment :: Pos -> Picture
drawSegment pos = drawAtPos pos $ rectangleSolid tileSize tileSize

drawCherry :: Maybe Pos -> Float -> Picture
drawCherry c worldTime = case c of 
    (Just pos) -> drawAtPos pos $ color rose $ circleSolid (tileSize * (0.55 + 0.1 * sin (worldTime * 5.0)))
    Nothing -> blank

drawAtPos :: Pos -> Picture -> Picture
drawAtPos (x,y) = translate (fromIntegral x * tileSize) (fromIntegral y * tileSize)

-- Updating

tick :: Float -> World -> World
tick dt = maybeCreateCherry . maybeEatCherry . tickSnakePart dt . increaseWorldTime dt

increaseWorldTime :: Float -> World -> World
increaseWorldTime dt world = world { time = time world + dt }

tickSnakePart :: Float -> World -> World
tickSnakePart dt w = w { snake = updateSnake dt (snake w) }

updateSnake :: Float -> Snake -> Snake
updateSnake dt = checkSelfCollision . checkBounds . maybeMove . increaseMoveTimer dt

increaseMoveTimer :: Float -> Snake -> Snake
increaseMoveTimer dt s = s { moveTimer = moveTimer s + dt }

maybeMove :: Snake -> Snake
maybeMove s = if moveTimer s > stepTime && alive s then move s else s

move :: Snake -> Snake
move s = s { body = body', moveTimer = 0.0 }
    where body' = newHead : init (body s)
          newHead = (oldHeadX + dirX, oldHeadY + dirY)
          (dirX, dirY) = dirToIntVector (dir s)
          (oldHeadX, oldHeadY) = head (body s)

dirToIntVector :: Dir -> (Int,Int)
dirToIntVector dir = case dir of
    N -> (0, 1)
    E -> (1, 0)
    S -> (0, -1)
    W -> (-1, 0)

checkBounds :: Snake -> Snake
checkBounds s = 
    let (x,y) = head $ body s
        alive' = and [x >= 0, x <= boardSize, y >= 0, y <= boardSize]
    in  s { alive = alive' && alive s }

checkSelfCollision :: Snake -> Snake
checkSelfCollision s =
    let (p:_:ps) = body s -- ignore second element (hits it on eat)
        alive' = p `notElem` ps
    in s { alive = alive' && alive s }

maybeCreateCherry :: World -> World
maybeCreateCherry world =
    case cherry world of
        (Just _) -> world -- cherry exists
        Nothing -> createCherryAtRandomPosition world

createCherryAtRandomPosition :: World -> World
createCherryAtRandomPosition = execState place
    where place = do randPos <- getRandomPosition
                     modify (addCherry randPos)

addCherry :: Pos -> World -> World
addCherry pos world = world { cherry = Just pos }

getRandomPosition :: State World Pos
getRandomPosition = do rx <- getRand (0, boardSize)
                       ry <- getRand (0, boardSize)
                       return (rx,ry)

maybeEatCherry :: World -> World
maybeEatCherry world =
    case cherry world of
        Nothing -> world
        (Just c) -> 
            let s = snake world
                snakeBody = body s
                snakeHead = head snakeBody
                eaten = snakeHead == c
                c' = if eaten then Nothing else Just c
                snakeBody' = if eaten then c : snakeBody else snakeBody
                s' = s { body = snakeBody' }
            in world { snake = s', cherry = c' }

-- Events

onEvent :: Event -> World -> World
onEvent (EventKey (SpecialKey KeySpace) Down _ _) world = restart world
onEvent (EventKey (SpecialKey key) Down _ _) world = controlSnake world key
onEvent _ world = world

restart :: World -> World
restart world = makeWorld (randg world)

controlSnake :: World -> SpecialKey -> World
controlSnake world key = 
    let s = snake world
    in  world { snake = changeDir s (controlDir key s) }

controlDir :: SpecialKey -> Snake -> Dir
controlDir key s =
    let currentDir = dir s
        (Just opposite) = lookup currentDir opposites
        desiredDir = case key of
                        KeyUp -> N
                        KeyRight -> E
                        KeyDown -> S
                        KeyLeft -> W
    in if desiredDir == opposite then currentDir else desiredDir

opposites :: [(Dir, Dir)]
opposites = [(N,S), (S,N), (E,W), (W,E)]

changeDir :: Snake -> Dir -> Snake
changeDir s newDir = s { dir = newDir }

-- Randomness

getRand :: (Int, Int) -> State World Int
getRand range = 
    do world <- get
       let g = randg world
           (r, g') = randomR range g
       put world { randg = g' }
       return r
