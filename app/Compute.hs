module Compute where

import Types
import Control.Parallel.Strategies
import Control.DeepSeq


getParticleData :: ParticleState -> (Float, Float, Float, Float)
getParticleData pS = (xPos pS, yPos pS, xVel pS, yVel pS)


updateState ::  Float -> Config -> ParticleState -> ParticleState
updateState dt cG prevState = 
  prevState {xPos = xPrev + vxPrev * dt, yPos = yPrev + vyNew * dt, yVel = vyNew}
  where 
    (xPrev, yPrev, vxPrev, vyPrev) = getParticleData prevState
    vyNew = vyPrev - gr * dt
    gr = g cG


inWall :: Float -> Float -> Config -> Maybe Wall
inWall x y cG
  | x - r < lWall = Just LeftWall 
  | x + r > rWall = Just RightWall
  | y + r > tWall = Just TopWall
  | y - r < bWall = Just BottomWall
  | otherwise = Nothing
  where 
    r = fromIntegral (radius cG) :: Float
    (lWall, rWall, tWall, bWall) = getWallLocs cG


adjustForWallBounce :: [ParticleState] -> Config -> [ParticleState]
adjustForWallBounce pSl cG = map bounce pSl
  where 
    bounce :: ParticleState -> ParticleState
    bounce pS =
      case inWall x y cG of
        Just LeftWall   -> 
          ParticleState (x+2*(lWall-x+r)) y (al*(negate vx)) (be*vy)
        Just RightWall  ->
          ParticleState (x-2*(x+r-rWall)) y (al*(negate vx)) (be*vy)
        Just TopWall    ->
          ParticleState x (y-2*(y+r-tWall)) (be*vx) (al*(negate vy))
        Just BottomWall ->
          ParticleState x (y+2*(bWall-y+r)) (be*vx) (al*(negate vy))
        Nothing         -> pS
      where
      (x, y, vx, vy) = getParticleData pS
      (al, be) = (alpha cG, beta cG)
      r = fromIntegral (radius cG) ::Float
      (lWall, rWall, tWall, bWall) = getWallLocs cG


hyp :: Float -> Float -> Float
hyp a b = sqrt $ a * a + b * b


collision :: ParticleState -> ParticleState -> Config -> (ParticleState, ParticleState)
collision pS1 pS2 cG
  | d >= 2 * r = (pS1, pS2)
  | otherwise = (new1, new2)
  where
    (al, be) = (alpha cG, beta cG)
    r = (fromIntegral (radius cG)) :: Float
    d = hyp (x2-x1) (y2-y1)
    (x1,y1,vx1,vy1) = getParticleData pS1
    (x2,y2,vx2,vy2) = getParticleData pS2
    v1 = (vx1, vy1)
    v2 = (vx2, vy2)
    n = (x2 - x1, y2 - y1)
    thetaN = posAtan2 n
    v1trans = transform (vx1, vy1) ((posAtan2 v1) - thetaN)
    v2trans = transform (vx2, vy2) ((posAtan2 v2) - thetaN)
    (v1trans', v2trans') = momentumTransfer v1trans v2trans
    (vx1', vy1') = transform v1trans' (thetaN + posAtan2 v1trans')
    (vx2', vy2') = transform v2trans' (thetaN + posAtan2 v2trans')
    pen = 2 * r - d
    new1 = ParticleState x1 y1 vx1' vy1'
    new2 = ParticleState (x2 + pen * cos thetaN) (y2 + pen * sin thetaN) vx2' vy2'
    posAtan2 :: (Float, Float) -> Float
    posAtan2 (x, y)
      | res < 0   = 2 * pi + res
      | otherwise = res
      where res = atan2 y x
    transform :: (Float, Float) -> Float -> (Float, Float)
    transform (x, y) phi = (mag * (cos phi), mag * (sin phi))
      where mag = hyp x y
    momentumTransfer :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
    momentumTransfer (vn1, vt1) (vn2, vt2) = ((al * vn2, be * vt1), (al * vn1, be * vt2))


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs
    

chunk :: Int -> [a] -> [[a]] 
chunk _ [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs


adjustForCollisions :: [ParticleState] -> Config -> [ParticleState]
adjustForCollisions [] _         = []
adjustForCollisions (pS1:[])  _  = [pS1]
adjustForCollisions (pS1:end) cG = pS1New:(adjustForCollisions endNew cG)
  where
    (pS1New, endNew) = helper pS1 end
    helper :: ParticleState -> [ParticleState] -> (ParticleState, [ParticleState])
    helper pS [] = (pS, [])
    helper pS (x:xs) = (a, newX : r)
      where
        (newPS, newX) = collision pS x cG
        (a, r) = helper newPS xs


adjustForCollisions2 :: [ParticleState] -> Config -> [ParticleState]
adjustForCollisions2 pSl cG = map helper pSl
  where
    helper :: ParticleState -> ParticleState
    helper pS = foldl helper2 pS pSl
      where
        helper2 :: ParticleState -> ParticleState -> ParticleState
        helper2 s e
          | e == pS     = s
          | first == pS = s
          | otherwise   = first
            where (first, _) = collision pS e cG


adjustForCollisions2Chunked :: [ParticleState] -> Config -> Int -> [ParticleState]
adjustForCollisions2Chunked pSl cG numChunks = concat (map (map helper) splitted `using` parList rdeepseq)
  where
    splitted = split numChunks pSl
    helper :: ParticleState -> ParticleState
    helper pS = foldl helper2 pS pSl
      where
        helper2 :: ParticleState -> ParticleState -> ParticleState
        helper2 s e
          | e == pS     = s
          | first == pS = s
          | otherwise   = first
            where (first, _) = collision pS e cG


nextStep :: Float -> Config -> [ParticleState] -> Int -> [ParticleState]
nextStep dt config currStates _ = force $ adjustForWallBounce stepped config
  where
    postCollisions, stepped :: [ParticleState]
    postCollisions = adjustForCollisions currStates config
    stepped = map (updateState dt config) postCollisions


nextStepChunkedForce :: Float -> Config -> Int -> [ParticleState] -> Int -> [ParticleState]
nextStepChunkedForce dt config numChunks currStates _ = 
  force $ adjustForWallBounce stepped config
    where
      postCollisions, stepped :: [ParticleState]
      postCollisions = adjustForCollisions currStates config
      splitted = split numChunks postCollisions
      stepped = concat (map (map (updateState dt config)) splitted `using` parList rseq)


nextStepChunkedDeep :: Float -> Config -> Int -> [ParticleState] -> Int -> [ParticleState]
nextStepChunkedDeep dt config numChunks currStates _ = 
  adjustForWallBounce stepped config
    where
      postCollisions, stepped :: [ParticleState]
      postCollisions = adjustForCollisions currStates config
      splitted = split numChunks postCollisions
      stepped = concat (map (map (updateState dt config)) splitted `using` parList rdeepseq)


nextStepParCollision :: Float -> Config -> Int -> [ParticleState] -> Int -> [ParticleState]
nextStepParCollision dt config numChunks currStates _ = force $ adjustForWallBounce stepped config
  where
    postCollisions, stepped :: [ParticleState]
    postCollisions = adjustForCollisions2Chunked currStates config numChunks
    stepped = map (updateState dt config) postCollisions
      

compute :: [ParticleState] -> Float -> Int -> Config -> [ParticleState]
compute initial dt nSteps config = 
  foldl (nextStep dt config) initial [1..nSteps]


computeMatrix :: [ParticleState] -> Float -> Int -> Config -> [[ParticleState]]
computeMatrix initial dt nSteps config = 
  reverse $ foldl helper [initial] [1..nSteps]
    where
      helper :: [[ParticleState]] -> Int -> [[ParticleState]]
      helper matrix@(front:_) step = (nextStep dt config front step) : matrix
      helper _ _ = error "computeMatrix helper error"
