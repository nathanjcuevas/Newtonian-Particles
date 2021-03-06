module Main where

import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Animate
import Compute
import Parse
import Types


maxSize :: Int
maxSize = 7000000


extractPosVectors :: [[ParticleState]] -> [[PosVector]]
extractPosVectors pSll = map helper pSll
  where 
    helper :: [ParticleState] -> [PosVector]
    helper pSl = map helper2 pSl
    helper2 :: ParticleState -> PosVector
    helper2 pS = PosVector (xPos pS) (yPos pS)


main :: IO ()
main = 
  do 
    args <- getArgs
    (filename, configPath, animate) <- 
      case args of
        [f, c] -> 
          return (f, c, False)
        [f, c, "-animate"] ->
          return (f, c, True)
        _   -> 
          do 
            pn <- getProgName
            die $ "Usage: "++pn++" <filename> <config> [-animate]"
    contents <- readFile filename
    configContents <- readFile configPath
    let 
      config = extractConfig configContents
      preset = contentsToData contents
      m = length preset
      steps = (totTime config) * (fps config)
      dt = 1 / (fromIntegral (fps config))
    if steps * m > maxSize then
      error "exceeded max size"
    else if animate then
      runAnimation (extractPosVectors (computeMatrix preset dt steps config)) config
    else
      print $ compute preset dt steps config
      where
