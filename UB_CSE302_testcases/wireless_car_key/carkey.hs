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

locked_func' :: Choreo IO (Bool @ "car")
locked_func' = car `locally` \un -> return True

challenge' :: Choreo IO (String @ "car")
challenge' = car `locally` \un -> return "Solve the challenge: 1+1 = ?\n"

key_present :: Bool
key_present = False

car_key :: Choreo IO ()
--should be done in 100ms, if not restart main
car_key = do
    locked <- locked_func'
    cond (car, locked) \case
        True -> do
            --car locked, try to send wake signal to key
            --get info if key present
            -- (\_ do : warp)
            -- (\un do : unwarp)
            key_receive_wake <- (car, \_ -> do return "myKey") ~~> key
            match <- key `locally` \un -> do return $ (un key_receive_wake) == "myKey"
            cond (key, match) \case
                True -> do
                    --key send out present signal
                    key_present <- (key, \_ -> do return True) ~~> car
                    key `locally` \un -> do putStrLn "Car present"
                False -> do
                    --key not present, rec main
                    --how to get start from the beginning?
                return car_key()

            --check if key near the car
            if_present <- car `locally` \un -> return key_present
            cond (car, if_present) \case
                True -> do
                --key send present signal
                --start encode & decode
                --(~~>) is variant of (~>) that allows to send the result of a local computation
                    encoded_challenge <- (car, \_ -> do return challenge') ~~> key
                    key_receive_challenge <- key `locally` \un -> do return $ encoded_challenge
                    key_answer <- (key, \_ -> do
                                putStrLn $ show (key_receive_challenge)
                                getLine
                            )
                            ~~> car
                    
                    match <- car `locally` \un -> return $ (un key_answer) == "2"
                    cond (car, match) \case
                        True -> do
                            --matched, unlock the car
                            locked <- car `locally` \un -> return False
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
                match <- car `locally` \un -> return $ (un lock_signal) == "LOCK"
                cond(car, match) \case
                    True -> do
                        --key still there
                        locked <- car `locally` \un -> return True
                        car_key()
                    False -> do
                        --key not present
                        locked <- car `locally` \un -> return False
                        car_key()

timeoutFunc :: IO ()
timeoutFunc = forever $ do
    let result = timeout (5000000) car_key() 
    case result of
        Nothing -> do
            putStrLn "Timeout"
            return timeoutFunc()
        Just _ -> do
            putStrLn "Incorrect/Car unlocked"
            return timeoutFunc()

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