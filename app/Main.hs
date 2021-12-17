module Main where

import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Animate
import Compute
import Parse
import Types


totalTime ,steps :: Int
totalTime = 30
steps = totalTime * fps

dt :: Float
dt = 1 / (fromIntegral fps)


extractPosVectors :: [[ParticleState]] -> [[PosVector]]
extractPosVectors pSll = map helper pSll
  where 
    helper :: [ParticleState] -> [PosVector]
    helper pSl = map helper2 pSl
    helper2 :: ParticleState -> PosVector
    helper2 pS = pos pS


test :: [[PosVector]]
test = 
  map helper [0..100 :: Float]
  where
    helper :: Float -> [PosVector] 
    helper d = [PosVector { xPos = 0 , yPos = negate d }]


main :: IO ()
main = 
  do 
    args <- getArgs
    filename <- 
      case args of
        [f] -> return f
        _   -> 
          do 
            pn <- getProgName
            die $ "Usage: "++pn++" <filename>"
    contents <- readFile filename
    runAnimation $ extractPosVectors $ compute (contentsToData contents) dt steps
