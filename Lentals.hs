{-# LANGUAGE TemplateHaskell #-}

module Lentals where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import System.Random
import Control.Monad.State

data Dot = Dot {
    _pos :: Point,
    _col :: Color,
    _rad :: Float
}
makeLenses ''Dot

data World = World {
    _dots :: [Dot],
    _randg :: StdGen
}
makeLenses ''World

lentals :: IO ()
lentals = do
    g <- newStdGen
    play (InWindow "Lentals" (400, 400) (100, 100)) white 100 (startWorld g) draw onEvent tick

startWorld :: StdGen -> World
startWorld g = World { _dots = [Dot (20,30) azure 20.0], _randg = g }

draw :: World -> Picture
draw world = pictures $ map drawDot (view dots world)

drawDot :: Dot -> Picture
drawDot dot = translateUsingPoint (dot^.pos) 
            $ color (dot^.col) 
            $ circleSolid (dot^.rad)

translateUsingPoint :: Point -> Picture -> Picture
translateUsingPoint (x,y) = translate x y

onEvent :: Event -> World -> World
onEvent (EventKey (MouseButton LeftButton) Down _ mousePos) world = execState (makeDot mousePos) world
onEvent _ world = world

makeDot :: Point -> State World ()
makeDot mousePos = do
    [rr,rb,rg] <- replicateM 3 (getRand (0.0, 1.0))
    radius <- getRand (10.0, 30.0)
    let dotColor = makeColor rr rb rg 1.0
    modify $ over dots (addDot dotColor mousePos radius)

addDot :: Color -> Point -> Float -> [Dot] -> [Dot]
addDot c p r ds = Dot { _pos = p, _col = c, _rad = r } : ds

tick :: Float -> World -> World
tick _ world = world

getRand :: Random a => (a, a) -> State World a
getRand range = 
    do world <- get
       let g = _randg world
           (r, g') = randomR range g
       put world { _randg = g' }
       return r

