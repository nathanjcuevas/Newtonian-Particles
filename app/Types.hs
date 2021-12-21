module Types where


data ParticleState = ParticleState
  { xPos :: !Float 
  , yPos :: !Float 
  , xVel :: !Float
  , yVel :: !Float
  } deriving Show


data PosVector = PosVector
  { xComp :: !Float
  , yComp :: !Float
  } deriving Show


data Wall = 
    LeftWall
  | RightWall
  | TopWall
  | BottomWall
  deriving Show


data Config = Config
  { g     :: !Float
  , alpha :: !Float
  , beta  :: !Float
  } deriving Show


defaultConfig :: Config
defaultConfig = 
  Config { g = 9.81, alpha = 0.93, beta = 0.98 }


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

