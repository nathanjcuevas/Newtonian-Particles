module Compute where

import Types
import Data.List
import Debug.Trace


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


getConfigData :: Config -> (Float, Float, Float)
getConfigData cG = (g cG, alpha cG, beta cG)


updateState ::  Float -> Config -> ParticleState -> ParticleState
updateState dt cG prevState = createParticleState xNew yNew vxNew vyNew
  where 
    (xPrev, yPrev, vxPrev, vyPrev) = getParticleData prevState
    xNew = xPrev + vxNew * dt
    yNew = yPrev + vyNew * dt
    vxNew = vxPrev
    vyNew = vyPrev - g * dt
    (g, _, _) = getConfigData cG


inWall :: Float -> Float -> Maybe Wall
inWall x y
  | x - r < leftWallLoc   = Just LeftWall
  | x + r > rightWallLoc  = Just RightWall
  | y + r > topWallLoc    = Just TopWall
  | y - r < bottomWallLoc = Just BottomWall
  | otherwise = Nothing
  where r = (fromIntegral radius) :: Float


adjustForWallBounce :: [ParticleState] -> Config -> [ParticleState]
adjustForWallBounce pSl cG = map bounce pSl
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
      (_, alpha, beta) = getConfigData cG
      r = (fromIntegral radius) :: Float


collision :: ParticleState -> ParticleState -> Config -> (ParticleState, ParticleState)
collision pS1 pS2 cG
  | d >= 2 * r = (pS1, pS2)
  | otherwise = (new1, new2)
  where
    (_, alpha, beta) = getConfigData cG
    r = (fromIntegral radius) :: Float
    d = sqrt $ (x2-x1)^2 + (y2-y1)^2
    (x1,y1,vx1,vy1) = getParticleData pS1
    (x2,y2,vx2,vy2) = getParticleData pS2
    nx = x2 - x1
    ny = y2 - y1
    thetaN = posAtan2 ny nx
    theta1 = posAtan2 vy1 vx1
    theta2 = posAtan2 vy2 vx2
    phi1 = theta1 - thetaN
    phi2 = theta2 - thetaN
    mag1 = sqrt $ vx1^2 + vy1^2
    mag2 = sqrt $ vx2^2 + vy2^2
    vn1 = mag1 * (cos phi1) 
    vt1 = mag1 * (sin phi1)
    vn2 = mag2 * (cos phi2)
    vt2 = mag2 * (sin phi2)
    vt2' = beta * vt2
    vt1' = beta * vt1
    vn1' = alpha * vn2
    vn2' = alpha * vn1
    mag1' = sqrt $ vn1'^2 + vt1'^2
    mag2' = sqrt $ vn2'^2 + vt2'^2
    phi1' = posAtan2 vt1' vn1'
    phi2' = posAtan2 vt2' vn2'
    theta1' = thetaN + phi1'
    theta2' = thetaN + phi2'
    angle = posAtan2 ny nx
    pen = 2 * r - d
    new1 = createParticleState x1 y1 (mag1' * (cos theta1')) (mag1' * (sin theta1'))
    new2 = createParticleState (x2 + pen * cos angle) (y2 + pen * sin angle) (mag2' * (cos theta2')) (mag2' * (sin theta2'))
    posAtan2 :: Float -> Float -> Float
    posAtan2 y x
      | res < 0   = 2 * pi + res
      | otherwise = res
      where res = atan2 y x


adjustForCollisions :: [ParticleState] -> Config -> [ParticleState]
adjustForCollisions (pS1:[])  _  = [pS1]
adjustForCollisions (pS1:rem) cG = pS1New:(adjustForCollisions remNew cG)
  where
    (pS1New, remNew) = helper pS1 rem
    helper :: ParticleState -> [ParticleState] -> (ParticleState, [ParticleState])
    helper pS [] = (pS, [])
    helper pS (x:xs) = (a, newX : r)
      where
        (newPS, newX) = collision pS x cG
        (a, r) = helper newPS xs


nextStep :: Float -> Config -> [[ParticleState]] -> Int -> [[ParticleState]]
nextStep dt config matrix@(currStates:_) step = result : matrix
  where
    result, postCollisions, stepped :: [ParticleState]
    postCollisions = adjustForCollisions currStates config
    stepped = map (updateState dt config) postCollisions
    result = adjustForWallBounce stepped config


compute :: [ParticleState] -> Float -> Int -> Config -> [[ParticleState]]
compute initial dt nSteps config = 
  reverse $ foldl (nextStep dt config) [initial] [1..nSteps]
