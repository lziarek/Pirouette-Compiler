{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Control.Monad
import Data.Proxy
import System.Environment

-- Step 1: Defining locations

agc :: Proxy "AGC"
agc = Proxy

lpf :: Proxy "LPF"
lpf = Proxy


-- Step 2: Writing a choreography
agcLpf :: Float @ "AGC" -> Float @ "AGC" -> Choreo IO (exitValue @ "LPF")  
agcLpf k1 k2 = do
     val1 <- (agc, k1) ~> lpf
     threadDelay 40000
     val2 <- (agc, k2) ~> lpf
     exitValue <- lpf `locally` (\x y -> return ((x + y) / 2)) val1 val2
     threadDelay 40000
     agcLpf



-- Step 3: Projecting and running the chreography
main :: IO () --ask for input from user 
main = do
  args <- getArgs
  case args of
    [loc] -> void $ runChoreography cfg choreography loc
    _     -> error "wrong usage: must provide exactly one location"
  where
    -- Step 4: Mapping locations to HTTP ports
    cfg = mkHttpConfig [ ("alice", ("localhost", 4242))
                       ]
