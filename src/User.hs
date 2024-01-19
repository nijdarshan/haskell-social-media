{-# LANGUAGE RecordWildCards #-}

module User (User, createUser, simulateSocialNetworkUser, readMailbox, sendMessage) where

import Control.Concurrent
import Control.Monad (when)
import System.Random
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, check)
import Types
import Faker (Fake, FakerSettings, defaultFakerSettings, generateWithSettings, setRandomGen)
import Faker.Quote.Shakespeare (hamlet)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)

-- | Selects a random user from the given list.
--
-- This function generates a random index and uses it to select a user from the list.
randomUser :: [User] -> IO User
randomUser users = do
  randomIndex <- randomRIO (0, length users - 1)
  return (users !! randomIndex)

-- | Creates a new user with the given username.
--
-- This function creates a new user with an empty mailbox.
createUser :: String -> IO User
createUser name = do
  box <- newMVar []
  return User {username = name, mailbox = box}

-- | Sends a message from one user to another.
--
-- This function creates a new message with the given content and adds it to the receiver's mailbox.
sendMessage :: User -> User -> String -> IO ()
sendMessage sender receiver content = do
  timestamp <- getCurrentTime
  let newMessage = Message {sender = username sender, receiver = username receiver, content = content, timestamp = timestamp}
  modifyMVar_ (mailbox receiver) (\messages -> return (newMessage : messages))

-- | Reads all messages from the user's mailbox.
--
-- This function returns all messages from the user's mailbox.
readMailbox :: User -> IO [Message]
readMailbox user = readMVar (mailbox user)

-- | Simulates the behavior of a social network user.
--
-- This function simulates the behavior of a user on a social network.
-- It periodically selects a random user from the given list and sends them a message.
-- The content of the message is a random quote from Shakespeare's Hamlet.
-- The function stops sending messages when the counter reaches 100.
simulateSocialNetworkUser :: User -> [User] -> TVar Int -> IO ()
simulateSocialNetworkUser user users counter = do
  let loop = do
        delay <- randomRIO (1, 5)
        threadDelay (delay * 1000000)  -- Sleep for delay seconds.

        receiverIndex <- randomRIO (0, length users - 1)
        let receiver = users !! receiverIndex

        shouldSend <- atomically $ do
          count <- readTVar counter
          check (count < 100)
          if user /= receiver then do -- Don't send messages to yourself.
            writeTVar counter (count + 1)
            return True
          else
            return False

        when shouldSend $ do
          gen <- newStdGen
          let settings = setRandomGen gen defaultFakerSettings
          quote <- generateWithSettings settings hamlet
          putStrLn $ "Sending message from " ++ username user ++ " to " ++ username receiver
          sendMessage user receiver (unpack quote)

        count <- atomically $ readTVar counter
        when (count >= 100) $ do
          putStrLn ("100 messages sent! Visit http://localhost:3000/ for more details." ++ "\n")
          putStrLn $ "Press Ctrl+c or Command+c to quit" ++ "\n"
        loop
        
  loop