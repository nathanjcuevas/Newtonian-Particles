module Animate where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Types


offset :: Int
offset = 100


getWindowFromConfig :: Config -> Display
getWindowFromConfig c = 
  InWindow "Particles" (width c, height c) (offset, offset)


background :: Color
background = black


update :: ViewPort -> Float -> [[PosVector]] -> [[PosVector]]
update _ _ []     = []
update _ _ (_:ps) = ps


render :: Config -> [[PosVector]] -> Picture
render _  []    = blank   -- when the simulation is done, show a blank screen
render cG (p:_) = 
  pictures $ map getTranslation p
  where 
    getTranslation :: PosVector -> Picture
    getTranslation state = 
      translate x y $ c $ circleSolid r
        where 
          x = xComp state
          y = yComp state
          c = color red
          r = fromIntegral $ radius cG


runAnimation :: [[PosVector]] -> Config -> IO ()
runAnimation ds cG = 
  simulate (getWindowFromConfig cG) background (fps cG) ds (render cG) update
