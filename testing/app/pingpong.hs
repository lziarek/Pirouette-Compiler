{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Choreography
-- import Control.Monad

import Control.Monad.IO.Class ()
import Data.Proxy
import System.Environment

ping :: Proxy "ping"
ping = Proxy

pong :: Proxy "pong"
pong = Proxy

loop' :: (Int @ "ping") -> Choreo IO ()
loop' val = do
  cond (ping, val) \case
    0 -> return ()
    _ -> do
      -- ping sends the current value to the pong
      newValue <- (ping, val) ~> pong
      nv <- (pong `locally` (\un -> return $ (un newValue) - 1))
      pong `locally` \unwrap -> do putStrLn ("i: " ++ show (unwrap nv))
      newValue' <- (pong, nv) ~> ping
      loop' newValue'

pingpongpro :: Choreo IO ()
-- pingpongpro = loop 5
pingpongpro = do
  v <- (ping `locally` \_ -> return 100)
  loop' v

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "ping" -> runChoreography cfg pingpongpro "ping"
    "pong" -> runChoreography cfg pingpongpro "pong"
  return ()
  where
    cfg =
      mkHttpConfig
        [ ("ping", ("localhost", 8080)),
          ("pong", ("localhost", 8081))
        ]
