module Animate where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Types


offset :: Int
offset = 100


window :: Display
window = InWindow "Particles" (width, height) (offset, offset)


background :: Color
background = black


update :: ViewPort -> Float -> [[PosVector]] -> [[PosVector]]
update _ _ []     = []
update _ _ (_:ps) = ps


render :: [[PosVector]] -> Picture
render []    = blank   -- when the simulation is done, show a blank screen
render (p:_) = 
  pictures $ map getTranslation p
  where 
    getTranslation :: PosVector -> Picture
    getTranslation state = 
      translate x y $ c $ circleSolid $ fromIntegral radius
        where 
          x = xPos state
          y = yPos state
          c = color red


runAnimation :: [[PosVector]] -> IO ()
runAnimation ds = simulate window background fps ds render update
