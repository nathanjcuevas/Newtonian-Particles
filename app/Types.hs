module Types where
import Control.DeepSeq


data ParticleState = ParticleState
  { xPos :: !Float 
  , yPos :: !Float 
  , xVel :: !Float
  , yVel :: !Float
  } deriving Show


instance NFData ParticleState where
  rnf (ParticleState x' y' vx' vy') =
    rnf x' `seq` rnf y' `seq` rnf vx' `seq` rnf vy'


instance Eq ParticleState where
  pS1 == pS2 = 
    (xPos pS1 == xPos pS2) &&
    (yPos pS1 == yPos pS2) &&
    (xVel pS1 == xVel pS2) &&
    (yVel pS1 == yVel pS2)


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
  { g       :: !Float
  , alpha   :: !Float
  , beta    :: !Float
  , width   :: !Int
  , height  :: !Int
  , radius  :: !Int
  , fps     :: !Int
  , totTime :: !Int
  } deriving Show


defaultConfig :: Config
defaultConfig = 
  Config 
    { g       = 9.81
    , alpha   = 0.93
    , beta    = 0.93
    , width   = 740
    , height  = 740
    , radius  = 3
    , fps     = 45
    , totTime = 60
    }


getWallLocs :: Config -> (Float, Float, Float, Float)
getWallLocs c = 
  (negate rightWallLoc, rightWallLoc, topWallLoc, negate topWallLoc)
  where 
    rightWallLoc = fromIntegral $ (width c) `div` 2
    topWallLoc = fromIntegral $ (height c) `div` 2
