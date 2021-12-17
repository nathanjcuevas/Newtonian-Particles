module Compute where


import Types


updateState ::  Float -> ParticleState -> ParticleState
updateState dt prevState =
  ParticleState
    { pos = PosVector { xPos = xNew , yPos = yNew}
    , vel = VelVector { xVel = vxNew , yVel = vyNew}
    }
  where 
    xPrev = xPos $ pos prevState
    yPrev = yPos $ pos prevState
    vxPrev = xVel $ vel prevState
    vyPrev = yVel $ vel prevState
    xNew = xPrev + vxNew * dt
    yNew = yPrev + vyNew * dt
    vxNew = vxPrev
    vyNew = vyPrev - g * dt


{-
    two steps need to be done here:
    1. need to calculate new particle state (regardless of collision) 
    2. apply collisions (if any) and calculate new particle state
    3. need to do prev 2 steps for all states for current step
-}
nextStep :: Float -> [[ParticleState]] -> Int -> [[ParticleState]]
nextStep dt matrix@(currStates:_) step = result : matrix
  where
    result :: [ParticleState]
    result = map (updateState dt) currStates


compute :: [ParticleState] -> Float -> Int -> [[ParticleState]]
compute initial dt nSteps = 
  reverse $ foldl (nextStep dt) [initial] [1..nSteps]
