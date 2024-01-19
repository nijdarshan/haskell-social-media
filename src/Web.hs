{-# LANGUAGE OverloadedStrings #-}

module Web where

import Types (Message(..), User(..))
import User (User, readMailbox)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad (forM_)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, docTypeHtml, body, h1, h2, ul, li, toHtml, p)

generateHtml :: [User] -> [[Message]] -> Html
generateHtml users messages = docTypeHtml $ do
    h1 "Messages"
    forM_ (zip users messages) $ \(user, userMessages) -> do
        h2 . toHtml $ username user
        ul $ forM_ userMessages $ \message -> do
            li $ do
                p . toHtml $ "From: " ++ sender message
                p . toHtml $ "Content: " ++ content message
                p . toHtml $ "Received at: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (timestamp message)
        p . toHtml $ "Total messages received: " ++ show (length userMessages)

-- | Start the web server.
--
-- This function starts a web server that listens on port 3000.
-- It uses the given function to get the list of users.
-- For each request, it reads the mailbox of each user and generates an HTML document with the messages.
-- The HTML document is sent as the response to the request.
startWebServer :: IO [User] -> IO ()
startWebServer getUsers = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port ++ "..." ++ "\n"
    putStrLn $ "Please provide permissions to port 3000 to run the web server...Ignore if done already" ++ "\n"
    putStrLn $ "View messages at http://localhost:" ++ show port ++ "/" ++ "\n"
    putStrLn $ "Keep hitting the refresh to see new messages" ++ "\n"
    putStrLn $ "Press Ctrl+c or Command+c to quit" ++ "\n"
    run port $ \_ respond -> do
        users <- getUsers
        messages <- mapM readMailbox users
        let html = renderHtml $ generateHtml users messages
        respond $ responseLBS status200 [("Content-Type", "text/html")] html

