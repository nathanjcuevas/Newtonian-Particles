module Compute where

import Types


createParticleState :: Float -> Float -> Float -> Float -> ParticleState
createParticleState x y vx vy =
  ParticleState
    { pos = PosVector { xPos = x , yPos = y}
    , vel = VelVector { xVel = vx , yVel = vy}
    }


getParticleData :: ParticleState -> (Float, Float, Float, Float)
getParticleData pS = (x, y, vx, vy)
  where
    x = xPos $ pos pS
    y = yPos $ pos pS
    vx = xVel $ vel pS
    vy = yVel $ vel pS


updateState ::  Float -> ParticleState -> ParticleState
updateState dt prevState = createParticleState xNew yNew vxNew vyNew
  where 
    (xPrev, yPrev, vxPrev, vyPrev) = getParticleData prevState
    xNew = xPrev + vxNew * dt
    yNew = yPrev + vyNew * dt
    vxNew = vxPrev
    vyNew = vyPrev - g * dt


inWall :: Float -> Float -> Maybe Wall
inWall x y
  | x - r < leftWallLoc   = Just LeftWall
  | x + r > rightWallLoc  = Just RightWall
  | y + r > topWallLoc    = Just TopWall
  | y - r < bottomWallLoc = Just BottomWall
  | otherwise = Nothing
  where r = (fromIntegral radius) :: Float


adjustForWallBounce :: [ParticleState] -> [ParticleState]
adjustForWallBounce pSl = map bounce pSl
  where 
    bounce :: ParticleState -> ParticleState
    bounce pS =
      case inWall x y of
        Just LeftWall   -> 
          createParticleState (x+2*(leftWallLoc-x+r)) y (alpha*(negate vx)) (beta*vy)
        Just RightWall  ->
          createParticleState (x-2*(x+r-rightWallLoc)) y (alpha*(negate vx)) (beta*vy)
        Just TopWall    ->
          createParticleState x (y-2*(y+r-topWallLoc)) (beta*vx) (alpha*(negate vy))
        Just BottomWall ->
          createParticleState x (y+2*(bottomWallLoc-y+r)) (beta*vx) (alpha*(negate vy))
        Nothing         -> pS
      where
      (x, y, vx, vy) = getParticleData pS
      r = (fromIntegral radius) :: Float


nextStep :: Float -> [[ParticleState]] -> Int -> [[ParticleState]]
nextStep dt matrix@(currStates:_) step = result : matrix
  where
    result, stepped :: [ParticleState]
    result = adjustForWallBounce stepped
    stepped = map (updateState dt) currStates


compute :: [ParticleState] -> Float -> Int -> [[ParticleState]]
compute initial dt nSteps = 
  reverse $ foldl (nextStep dt) [initial] [1..nSteps]
