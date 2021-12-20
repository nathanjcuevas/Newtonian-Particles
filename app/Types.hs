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


data Wall = 
    LeftWall
  | RightWall
  | TopWall
  | BottomWall
  deriving Show


fps, width, height, radius :: Int
fps = 100
width = 500
height = 500
radius = 10


rightWallLoc, leftWallLoc, topWallLoc, bottomWallLoc :: Float
rightWallLoc = fromIntegral $ width `div` 2
leftWallLoc = negate rightWallLoc
topWallLoc = fromIntegral $ height `div` 2
bottomWallLoc = negate topWallLoc


g, alpha, beta :: Float
g = 9.81
alpha = 0.98
beta = 0.92
