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
  response <- server 'locally' \un -> delayTime (un request') (un state)
  (server, response) ~> ping

-- keeping the delayTime because not sure about line 10.
delayTime :: Request -> IORef State -> IO Response

threadDelay 2000000
return

mainChoreo :: Int -> Choreo IO ()
mainChoreo state = loop state -- start loop with i = 5
  where
    loop :: Int -> Choreo IO ()
    loop 0 = terminate -- when state = 0, terminate the loop
    loop state = do
      request <- ping `locally` \_ -> readRequest
      response <- pingpong request state
      loop (state - 1) -- recursive call where state is decremented

main :: IO ()
main = runChoreo $ mainChoreo 5