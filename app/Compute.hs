module Compute where

import Types
import Data.List
import Debug.Trace
import Control.Parallel.Strategies
import Control.DeepSeq


getParticleData :: ParticleState -> (Float, Float, Float, Float)
getParticleData pS = (xPos pS, yPos pS, xVel pS, yVel pS)


getConfigData :: Config -> (Float, Float, Float)
getConfigData cG = (g cG, alpha cG, beta cG)


updateState ::  Float -> Config -> ParticleState -> ParticleState
updateState dt cG prevState = 
  prevState {xPos = xPrev + vxPrev * dt, yPos = yPrev + vyNew * dt, yVel = vyNew}
  where 
    (xPrev, yPrev, vxPrev, vyPrev) = getParticleData prevState
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
          ParticleState (x+2*(leftWallLoc-x+r)) y (alpha*(negate vx)) (beta*vy)
        Just RightWall  ->
          ParticleState (x-2*(x+r-rightWallLoc)) y (alpha*(negate vx)) (beta*vy)
        Just TopWall    ->
          ParticleState x (y-2*(y+r-topWallLoc)) (beta*vx) (alpha*(negate vy))
        Just BottomWall ->
          ParticleState x (y+2*(bottomWallLoc-y+r)) (beta*vx) (alpha*(negate vy))
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
    new1 = ParticleState x1 y1 (mag1' * (cos theta1')) (mag1' * (sin theta1'))
    new2 = ParticleState (x2 + pen * cos angle) (y2 + pen * sin angle) (mag2' * (cos theta2')) (mag2' * (sin theta2'))
    posAtan2 :: Float -> Float -> Float
    posAtan2 y x
      | res < 0   = 2 * pi + res
      | otherwise = res
      where res = atan2 y x


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs
    

chunk :: Int -> [a] -> [[a]] 
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs


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


nextStep :: Float -> Config -> [ParticleState] -> Int -> [ParticleState]
nextStep dt config currStates step = force $ adjustForWallBounce stepped config
  where
    postCollisions, stepped :: [ParticleState]
    postCollisions = adjustForCollisions currStates config
    stepped = map (updateState dt config) postCollisions

{-
nextStep :: Float -> Config -> [ParticleState] -> Int -> [ParticleState]
nextStep dt config currStates step = result
  where
    postCollisions = adjustForCollisions currStates config
    (arr1, arr4) = splitAt 25 postCollisions
    (arr2, arr3) = splitAt 25 arr4
    arrs = foldl (++) [] [ map (updateState dt config) arr1
                         , map (updateState dt config) arr2
                         , map (updateState dt config) arr3] `using` parList rseq
    result = force $ adjustForWallBounce arrs config
-}

nextStepChunkedForce_strat :: Float -> Config -> Int -> [ParticleState] -> Int -> [ParticleState]
nextStepChunkedForce_strat dt config numChunks currStates step = 
  force $ adjustForWallBounce stepped config
    where
      postCollisions, stepped :: [ParticleState]
      postCollisions = adjustForCollisions currStates config
      splitted = split numChunks postCollisions
      stepped = concat (map (map (updateState dt config)) splitted `using` parList rseq)


nextStepChunkedDeep_strat :: Float -> Config -> Int -> [ParticleState] -> Int -> [ParticleState]
nextStepChunkedDeep_strat dt config numChunks currStates step = 
  adjustForWallBounce stepped config
    where
      postCollisions, stepped :: [ParticleState]
      postCollisions = adjustForCollisions currStates config
      splitted = split numChunks postCollisions
      stepped = concat (map (map (updateState dt config)) splitted `using` parList rdeepseq)
      

compute :: [ParticleState] -> Float -> Int -> Config -> [ParticleState]
compute initial dt nSteps config = 
  foldl (nextStepChunkedDeep_strat dt config 7) initial [1..nSteps]


computeMatrix :: [ParticleState] -> Float -> Int -> Config -> [[ParticleState]]
computeMatrix initial dt nSteps config = 
  reverse $ foldl helper [initial] [1..nSteps]
    where
      helper :: [[ParticleState]] -> Int -> [[ParticleState]]
      helper matrix@(front:_) step = (nextStep dt config front step) : matrix
