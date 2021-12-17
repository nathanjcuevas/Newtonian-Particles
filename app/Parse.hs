module Parse where


import Compute


splitComma :: String -> [String]
splitComma s = 
  case span (/= ',') s of
    (start , "")      -> [start]
    (start , ',':rem) -> start:(splitComma rem)
    _                 -> error "parse error in splitComma"


listToParticleState :: [String] -> ParticleState
listToParticleState (x:y:vx:vy:[]) = 
  ParticleState 
    { xPos = read x
    , yPos = read y
    , xVel = read vx
    , yVel = read vy
    }
listToParticleState _ = error "mismatched dimensions in file"


contentsToData :: String -> [ParticleState]
contentsToData contents =
  map (listToParticleState . splitComma) $ words contents
