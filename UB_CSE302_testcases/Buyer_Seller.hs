{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeOperators  #-}

module Main where

import           Choreography       (Choreo, cond, locally, mkHttpConfig,
                                     runChoreography, type (@), (~>))
import           Control.Monad      (void)
import           Data.Proxy         (Proxy (..))
import           System.Environment (getArgs)

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

auctionItem :: String
auctionItem = "Example Item"

highestBid :: Int
highestBid =   100

auction :: Choreo IO (Maybe Int @ "buyer")
auction = do
    -- The buyer proposes a bid. This is a local computation at the buyer's location, which is represented by the underscore (_) in the lambda function.
    -- The bid is returned as a located value at the buyer's location.
    proposedBid <- buyer `locally` \_ -> do
        putStrLn "Please enter your bid amount:"
        read <$> getLine

    proposedBid' <- (buyer, proposedBid) ~> seller

    -- The seller makes a decision based on the proposed bid. They compare it to the highest bid and return a boolean indicating whether the proposed bid is greater.
    decision <- seller `locally` \un -> return $ un proposedBid' > highestBid

    -- Conditionally execute the following based on the seller's decision.
    cond (seller, decision) \case
        -- If the seller's decision is true, meaning the proposed bid is higher than the highest bid, the following actions occur:
        True  -> do
            newHighestBid <- (seller, proposedBid') ~> buyer
            -- At the buyer's location, a message is printed to inform them that their bid was accepted, and the new highest bid is returned.
            buyer `locally` \un -> do
                putStrLn $ "Your bid of " ++ show (un newHighestBid) ++ " has been accepted."
                return $ Just (un newHighestBid)
        -- If the seller's decision is false, the following actions occur:
        False ->
            -- At the buyer's location, a message is printed to inform them that their bid was rejected, and nothing is returned.
            buyer `locally` \_ -> do
                putStrLn "Your bid has been rejected."
                return Nothing

main :: IO ()
main = do
    [loc] <- getArgs
    void $ runChoreography cfg auction loc
    where
        cfg = mkHttpConfig
            [ ("buyer",     ("localhost",   4242))
            , ("seller",    ("localhost",   4343))
            ]