{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Choreography
import           Control.Monad
import           Control.Concurrent
import           Data.Proxy
import           System.Environment --(getArgs)
import           System.Timeout

car :: Proxy "car"
car = Proxy

key :: Proxy "key"
key = Proxy

carkey :: Choreo IO (Bool @ "car")
--should be done in 100ms, if not restart main
carkey = do
    locked <- car `locally` \un -> return True
    key_present <- car `locally` \un -> return False
    cond (car, locked) \case
        True -> do
            --car locked, try to send wake signal to key
            --get info if key present
            -- (\_ do : warp)
            -- (\un do : unwarp)
            key_receive_wake <- (car, \_ -> do return "myKey") ~~> key
            match <- key `locally` \un -> do return $ (un key_receive_wake) == "myKey"
            key_present <- (key, \_ -> do return True) ~~> car

            --check if key near the car
            cond (car, key_present) \case
                True -> do
                --key send present signal
                --start encode & decode
                --(~~>) is variant of (~>) that allows to send the result of a local computation
                    car `locally` \un -> do putStrLn "Key present"
                    encoded_challenge <- (car, \_ -> do return "Solve the challenge: 1+1 = ?\n") ~~> key
                    key `locally` \un -> do putStrLn $ (un encoded_challenge)
                    key_answer <- (key, \_ -> do
                            putStrLn "Enter the answer: "
                            getLine
                        )
                        ~~> car
                        
                    match <- car `locally` \un -> return $ (un key_answer) == "2"
                    cond (car, match) \case
                        True -> do
                            --matched, unlock the car
                            car `locally` \un -> do putStrLn "Unlock the car"
                            locked <- car `locally` \un -> return False
                            return $ (locked)
                        False -> do
                            --key not present
                            car `locally` \un -> do putStrLn "Incorrect, lock the car"
                            locked <- car `locally` \un -> return False
                            return $ (locked)

                False -> do
                        --key not present
                    car `locally` \un -> do putStrLn "Key not present"
                    locked <- car `locally` \un -> return False
                    return $ (locked)

        --car is unlock rn
        False -> do
            --receive key_present false signal if key lock the car
                lock_signal <- (key, \_ -> do
                            putStrLn "Car is unlocked, press LOCK to lock the car\n"
                            getLine
                        )
                        ~~> car
                match <- car `locally` \un -> return $ (un lock_signal) == "LOCK"
                cond(car, match) \case
                    True -> do
                        --key still there
                        locked <- car `locally` \un -> return True
                        return $ locked
                    False -> do
                        --key not present
                        locked <- car `locally` \un -> return False
                        return $ (locked)

-- HasChor do not have API to close http
-- infLoop :: IO ()
-- infLoop = do
--   [loc] <- getArgs
--   case loc of
--         "car" -> runChoreography cfg carkey "car"
--         "key" -> runChoreography cfg carkey "key"
--   return ()
--   where
--       cfg = mkHttpConfig [ ("car",  ("localhost", 4242))
--                          , ("key", ("localhost", 4343))
--                          ]

-- main :: IO ()
-- main = forever $ do
--    result <- timeout 5000000 infLoop
--    case result of
--        Nothing -> putStrLn "Timeout"
--        Just _ -> putStrLn "Incorrect/Car unlocked"
-- END

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
        "car" -> runChoreography cfg carkey "car"
        "key" -> runChoreography cfg carkey "key"
  return ()
  where
      cfg = mkHttpConfig [ ("car",  ("localhost", 4242))
                         , ("key", ("localhost", 4343))
                         ]
