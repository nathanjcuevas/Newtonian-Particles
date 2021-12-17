module Parse where


import Types


splitComma :: String -> [String]
splitComma s = 
  case span (/= ',') s of
    (start , "")      -> [start]
    (start , ',':rem) -> start:(splitComma rem)
    _                 -> error "parse error in splitComma"


listToParticleState :: [String] -> ParticleState
listToParticleState (x:y:vx:vy:[]) = 
  ParticleState 
    { pos = p , vel = v }
    where 
      p = PosVector { xPos = read x , yPos = read y }
      v = VelVector { xVel = read vx , yVel = read vy }
listToParticleState _ = error "mismatched dimensions in file"


contentsToData :: String -> [ParticleState]
contentsToData contents =
  map (listToParticleState . splitComma) $ words contents
