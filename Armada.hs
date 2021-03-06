module Armada where

import Graphics.Gloss

armada = play (InWindow "Armada" (1000, 600) (10, 10)) 
         black 100 startWorld draw onEvent tick

type Ship = (Float,Float)

startWorld :: [Ship]
startWorld = [(x,y) | x <- [400, 420.. 500], y <- [-250, -230..200]]

drawShip :: Ship -> Picture
drawShip (x,y) = translate x y $ color white $ circleSolid radius
    where radius = 10 * (sin (x * 0.01) + 2)

draw :: [Ship] -> Picture
draw world = pictures $ map drawShip world

onEvent _ world = world

moveLeft :: Float -> Ship -> Ship
moveLeft dt (x,y) = (x', y)
    where x' = x - dt * speed
          speed = 400 - y

wrap :: Ship -> Ship
wrap (x,y) = (x', y)
    where x' = if x < -510 then 510 else x

curve :: Ship -> Ship
curve (x,y) = (x, y')
    where y' = y + 1.0 * sin (x / 100)

updateShip :: Float -> Ship -> Ship
updateShip dt = wrap . curve . moveLeft dt

tick :: Float -> [Ship] -> [Ship]
tick dt world = map (updateShip dt) world