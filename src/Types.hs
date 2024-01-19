{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Concurrent
import Data.Time.Clock (UTCTime)

-- | The 'User' type represents a user in the social network.
data User = User {
  username :: String,  -- ^ The username of the user.
  mailbox :: MVar [Message]  -- ^ The user's mailbox.
} deriving (Eq)

-- | The 'Message' type represents a message sent from one user to another.
data Message = Message {
  sender :: String,  -- ^ The sender of the message.
  receiver :: String,  -- ^ The receiver of the message.
  content :: String,  -- ^ The content of the message.
  timestamp :: UTCTime
}
