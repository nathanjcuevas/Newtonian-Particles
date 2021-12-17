module Animate where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


data State = State   
  { xPos :: Float
  , yPos :: Float
  } deriving Show


data Particles = Particles
  { states :: [State]
  } deriving Show


width, height, offset, radius, fps :: Int
width = 500
height = 500
offset = 100
radius = 5
fps = 10


g, alpha :: Float
g = 9.81
alpha = 0.8


window :: Display
window = InWindow "Particles" (width, height) (offset, offset)


background :: Color
background = black


update :: ViewPort -> Float -> [Particles] -> [Particles]
update _ _ []     = []
update _ _ (_:ps) = ps


render :: [Particles] -> Picture
render []    = blank   -- when the simulation is done, show a blank screen
render (p:_) = 
  pictures $ map getTranslation $ states p
  where 
    getTranslation :: State -> Picture
    getTranslation state = 
      translate x y $ c $ circleSolid $ fromIntegral $ 2 * radius
        where 
          x = xPos state
          y = yPos state
          c = color red


runAnimation :: [Particles] -> IO ()
runAnimation ds = simulate window background fps ds render update
