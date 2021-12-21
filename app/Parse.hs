module Parse where

import Data.List
import Types


splitComma :: String -> [String]
splitComma s = 
  case span (/= ',') s of
    (start , "")      -> [start]
    (start , ',':rem) -> start:(splitComma rem)
    _                 -> error "parse error in splitComma"


listToParticleState :: [String] -> ParticleState
listToParticleState (x:y:vx:vy:[]) = 
  ParticleState (read x) (read y) (read vx) (read vy)
listToParticleState _ = error "mismatched dimensions in data file"


contentsToData :: String -> [ParticleState]
contentsToData contents =
  map (listToParticleState . splitComma) $ words contents


extractConfig :: String -> Config
extractConfig contents = 
  foldl helper defaultConfig $ splitComma contents
  where
    helper :: Config -> String -> Config
    helper c s
      | isPrefixOf "g=" s     = 
        c { g = stripPrefixFloat "g=" s }
      | isPrefixOf "alpha=" s = 
        c { alpha = stripPrefixFloat "alpha=" s }
      | isPrefixOf "beta="  s = 
        c { beta = stripPrefixFloat "beta=" s }
      | otherwise = error "invalid config file"
    stripPrefixFloat :: String -> String -> Float
    stripPrefixFloat prefix s= 
      case stripPrefix prefix s of 
        Just post -> read post
        Nothing   -> error "invalid config file"
