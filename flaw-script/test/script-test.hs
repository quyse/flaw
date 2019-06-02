{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Control.Concurrent
import Control.Monad

import Flaw.Book
import Flaw.Script

main :: IO ()
main = do
  (action1, action2) <- withBook $ \bk -> do
    interpreter <- book bk $ newInterpreter ["test"]
    action1 <- interpret interpreter ["Testscript1"] "main"
    action2 <- interpret interpreter ["Testscript2"] "main"
    action1
    action2
    return (action1, action2)
  action1
  action2
  void $ forkIO $ do
    threadDelay 1000000
    action1
  void $ forkIO $ do
    threadDelay 1000000
    action2
  threadDelay 2000000
