{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Choreography
import           Control.Monad
import           Control.Concurrent
import           Data.Proxy
import           System.Environment --(getArgs)

car :: Proxy "car"
car = Proxy

key :: Proxy "key"
key = Proxy

locked :: Bool
locked = True

challenge :: String
challenge = "Solve the challenge: 1+1 = ?\n"

answer :: String
answer = "2"

key_present :: Bool
key_present = False

car_key :: Choreo IO ()
--should be done in 100ms, if not restart main
car_key = do
    cond (car, locked) \case
        True -> do
            --car locked, try to send wake signal to key
            --get info if key present
            wake_msg <- (car, "myKEY") ~> key
            cond (key, wake_msg == "myKEY") \case
                True -> do
                    --key send out present signal
                    key_present <- (key, True) ~~> car
                    key `locally` \un -> do
                        putStrLn "Car present"
                False -> do
                    --key not present, rec main
                    car_key()

            --check if key near the car
            cond (car, key_present) \case
                True -> do
                --key send present signal
                --start encode & decode
                --(~~>) is variant of (~>) that allows to send the result of a local computation
                    encoded_challenge <- (car, challenge) ~> key
                    key_challenge <- (key, \_ -> do
                                putStrLn encoded_challenge
                                getLine
                            )
                            ~~> car
                    
                    cond (car, key_challenge == answer) \case
                        True -> do
                            --matched, unlock the car
                            let locked = False
                            car_key()
                        False -> do
                            --not matched, rec main
                            car_key()
                    
                False -> do
                    --not present, rec main
                    car_key()


        --car is unlock rn
        False -> do
            --receive key_present false signal if key lock the car
                lock_signal <- (key, \_ -> do
                            putStrLn "Car is unlocked, press LOCK to lock the car\n"
                            getLine
                        )
                        ~~> car
                cond(car, lock_signal == "LOCK") \case
                    True -> do
                        --key still there
                        let locked = True
                        car_key()
                    False -> do
                        --key not present
                        let locked = False
                        car_key()

timeoutFunc :: IO ()
timeoutFunc = forever $ do
    let result = timeout (10000000) car_key() 
    case result of
        Nothing -> do
            putStrLn "Timeout"
            timeoutFunc()
        Just _ -> do
            putStrLn "Incorrect/Car unlocked"
            timeoutFunc()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
        "buyer"  -> runChoreography cfg timeoutFunc "car"
        "seller" -> runChoreography cfg timeoutFunc "key"
    return ()
    where
        cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                            , ("seller", ("localhost", 4343))
                            ]