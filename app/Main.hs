module Main where

import Animate
import Compute
import Parse
import System.Environment(getArgs, getProgName)
import System.Exit(die)


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
    print $ contentsToData contents
