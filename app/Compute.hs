module Compute where


data ParticleState = ParticleState
  { xPos :: Double
  , yPos :: Double
  , xVel :: Double
  , yVel :: Double
  } deriving Show


compute :: [ParticleState] -> Double -> [(Double , Double)]
compute initial dt = [foo]
  where
    foo :: (Double , Double)
    foo = (0 , 0)

