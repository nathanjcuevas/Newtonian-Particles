module Main where

import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Data.Time (getCurrentTime, diffUTCTime)
import Animate
import Compute
import Parse
import Types


totalTime ,steps :: Int
totalTime = 60 * 15
steps = totalTime * fps

dt :: Float
dt = 1 / (fromIntegral fps)


configPath :: String
configPath = "config.csv"


extractPosVectors :: [[ParticleState]] -> [[PosVector]]
extractPosVectors pSll = map helper pSll
  where 
    helper :: [ParticleState] -> [PosVector]
    helper pSl = map helper2 pSl
    helper2 :: ParticleState -> PosVector
    helper2 pS = pos pS


main :: IO ()
main = 
  do 
    args <- getArgs
    (filename, noplot) <- 
      case args of
        [f] -> 
          return (f, False)
        [f, "-noplot"] ->
          return (f, True)
        _   -> 
          do 
            pn <- getProgName
            die $ "Usage: "++pn++" <filename> [-noplot]"
    contents <- readFile filename
    configContents <- readFile configPath
    let 
      config = extractConfig configContents
      vectors = extractPosVectors $ compute (contentsToData contents) dt steps config
      presets = extractConfig configContents
    if not noplot then
      runAnimation vectors
    else
      print $ head $ reverse vectors
