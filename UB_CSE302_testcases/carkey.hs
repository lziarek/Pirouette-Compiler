{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Choreography       (Choreo, cond, locally, mkHttpConfig,
                                     runChoreography, type (@), (~>))
import           Control.Monad      (void, forever)
import           Data.Proxy         (Proxy (..))
import           System.Environment --(getArgs)

car :: Proxy "car"
car = Proxy

key :: Proxy "key"
key = Proxy

locked :: Bool
locked = true

key_present :: Bool
key_present = false

challenge :: String
challenge = "passwd"

--encode :: String -> Choreo (String)
--decode :: Choreo (String) -> Choreo (String)

-- How to avoid infinite call of car_key() inside car_key()?
-- Do I need to return sth from car_key()?

car_key :: Choreo IO () (challenge @ car)
car_key = do
    cond (car, locked) \case
        --car is locked
        True -> do
            --car locked
            --try to send wake signal to key
            --get info if key present
            --should be done in 100ms, if not restart main
            let key_present = false
            locked' <- (car, wake()) ~> key
            key_present' <- (key, unlock()) ~> car

            --check if key near the car
            cond (key, key_present) \case
            False ->
                --not present, rec main
                car_key()

            True -> do
            --key send present signal
            --start encode & decode
            --(~~>) is variant of (~>) that allows to send the result of a local computation
            encoded_challenge <- (car, encode(challenge)) ~~> key
            decoded_challenge <- (key, decode(encoded_challenge)) ~~> car
            cond (car, encoded_challenge == challenge) \case
            True -> do
                --matched, unlock the car
                locked = false
                car_key()

        --car is unlock rn
        False -> do
            --receive key_present false signal if key lock the car
            key_present <- (key, lock()) ~~> car
            cond(key, key_present) \case
            True ->
                --key still there
                car_key()
            False ->
                --key not present
                locked = true
                car_key()

timeoutFunc :: IO ()
timeoutFunc = forever $ do
    result = timeout (100000) car_key() 
    case result of
        Nothing -> do
            putStrLn "Timeout"
            timeoutFunc()
        Just _ -> do
            putStrLn "Incorrect/Car unlocked"
            timeoutFunc()

main :: IO ()
main = do
    void $ runChoreography cfg timeoutFunc loc
    where
        cfg = mkHttpConfig
            [("car", "http://localhost:8080")
            , ("key", "http://localhost:8081")
            ]