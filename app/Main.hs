{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Data.List (nub)
import Data.Text (unpack)
import Faker (generateWithSettings, defaultFakerSettings, setRandomGen)
import Faker.Name (firstName, lastName)
import Types
import User (User, createUser, simulateSocialNetworkUser)
import Web (startWebServer)

-- | Creates a list of unique users.
--
-- This function generates a list of users with unique names.
-- It uses the Faker library to generate random first and last names.
-- If a generated name is already in use, it generates a new one.
loop :: [User] -> Int -> IO [User]
loop users 0 = return users
loop users n = do
    gen <- newStdGen
    let settings = setRandomGen gen defaultFakerSettings
    fName <- generateWithSettings settings firstName
    lName <- generateWithSettings settings lastName
    let userName = unpack fName ++ " " ++ unpack lName
    if userName `elem` (username <$> users)
        then loop users n
        else do
            user <- createUser userName
            loop (user:users) (n-1)

-- | The main function.
--
-- This function creates 10 users with unique names and starts a web server.
-- It also spawns a thread for each user to simulate their behavior on the social network.
main :: IO ()
main = do
    -- Create 10 users with unique names.
    users <- loop [] 10
    -- Create a shared TVar to count the number of messages sent.
    counter <- newTVarIO 0
    -- Spawn 10 threads, one for each user.
    forM_ users $ \user -> forkIO (simulateSocialNetworkUser user users counter)
    -- Start the web server.
    startWebServer (return users)
