{-# LANGUAGE BlockArguments #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Choreo

type State = Int

type Response = Maybe String

pingpong ::
  Request @ "ping" ->
  IORef State @ "pong" ->
  Choreo IO (Response @ "ping")
pingpong request state = do
  request' <- (ping, request) ~> server
  response <- server 'locally' \un -> return Nothing
  (server, response) ~> ping

mainChoreo :: Int -> Choreo IO ()
mainChoreo state = loop state -- start loop with i = 5
  where
    loop :: Int -> Choreo IO ()
    loop 0 = terminate -- when state = 0, terminate the loop
    loop state = do
      liftIO $ putStrLn $ "i: " ++ show (state - 1)
      request <- ping `locally` \_ -> readRequest
      response <- pingpong request state
      loop (state - 1) -- recursive call where state is decremented

main :: IO ()
main = runChoreo $ mainChoreo 5