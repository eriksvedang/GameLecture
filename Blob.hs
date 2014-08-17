module Blob where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

blob :: IO ()
blob = play (InWindow "The Blob" (400, 400) (10, 10)) 
       white 100 startWorld draw onEvent tick

startWorld :: Float
startWorld = 50.0

draw :: Float -> Picture
draw world = color rose $ circleSolid world

onEvent :: Event -> Float -> Float
onEvent _ world = world - 10.0

tick :: Float -> Float -> Float
tick dt world = world + dt * 50.0