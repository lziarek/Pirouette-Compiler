{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Data.Proxy
import System.Environment

-- Step 1: Defining locations

agc :: Proxy "agc"
agc = Proxy

lpf :: Proxy "lpf"
lpf = Proxy


-- Step 2: Writing a choreography
agcLpf :: Float @ "agc" -> Float @ "agc" -> Choreo IO (Float @ "lpf")  
agcLpf k1 k2 = do
     --wait 40000
     val1 <- (agc, k1) ~> lpf
     --wait 40000
     val2 <- (agc, k2) ~> lpf
     exitValue <- lpf `locally` \unwrap -> return $ (val1 + val2) / 2 
     lpf `locally` \un -> return exitValue



-- Step 3: Projecting and running the chreography
main :: IO () --ask for input from user 
main = do 
  [k1, k2] <- map read <$> getArgs
  config <- mkLocalConfig locs
  mapConcurrently_ (runChoreography config (agcLpf k1 k2)) locs
  return()
  where
    locs = ["agc", "lpf"]
