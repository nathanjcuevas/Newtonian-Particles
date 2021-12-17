module Types where


data ParticleState = ParticleState
  { pos :: PosVector
  , vel :: VelVector
  } deriving Show


data PosVector = PosVector
  { xPos :: Float 
  , yPos :: Float 
  } deriving Show


data VelVector = VelVector
  { xVel :: Float 
  , yVel :: Float 
  } deriving Show


fps, width, height, radius :: Int
fps = 20
width = 500
height = 500
radius = 5


g :: Float
g = 9.81
