module Compute where

import Types
import Data.List


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


collision :: ParticleState -> ParticleState -> (ParticleState, ParticleState)
collision pS1 pS2
  | d > 2 * r = (pS1, pS2)
  | otherwise = (new1, new2)
  where
    r = (fromIntegral radius) :: Float
    d = sqrt $ ((x2-x1)^ 2) + ((y2-y1)^2)
    (x1,y1,vx1,vy1) = getParticleData pS1
    (x2,y2,vx2,vy2) = getParticleData pS2
    power =  0.00482 * ((abs vx1) + (abs vy1) + (abs vx2) + (abs vy2))
    opposite = y1 - y2
    adjacent = x1 - x2
    rotation = atan2 opposite adjacent
    vel2x = 90 * power * cos (rotation + pi)
    vel2y = 90 * power * sin (rotation + pi)
    vel1x = 90 * power * cos rotation
    vel1y = 90 * power * sin rotation 
    new1 = createParticleState x1 y1 (vx1 + vel1x) (vy1 + vel1y)
    new2 = createParticleState x2 y2 (vx2 + vel2x) (vy2 + vel2y)


adjustForCollisions :: [ParticleState] -> [ParticleState]
adjustForCollisions (pS1:[]) = [pS1]
adjustForCollisions (pS1:rem) = pS1New:(adjustForCollisions remNew)
  where
    (pS1New, remNew) = helper pS1 rem
    helper :: ParticleState -> [ParticleState] -> (ParticleState, [ParticleState])
    helper pS [] = (pS, [])
    helper pS (x:xs) = (a, newX : r)
      where
        (newPS, newX) = collision pS x
        (a, r) = helper newPS xs


nextStep :: Float -> [[ParticleState]] -> Int -> [[ParticleState]]
nextStep dt matrix@(currStates:_) step = result : matrix
  where
    result, postCollisions, stepped :: [ParticleState]
    postCollisions = adjustForCollisions currStates
    stepped = map (updateState dt) postCollisions
    result = adjustForWallBounce stepped


compute :: [ParticleState] -> Float -> Int -> [[ParticleState]]
compute initial dt nSteps = 
  reverse $ foldl (nextStep dt) [initial] [1..nSteps]
