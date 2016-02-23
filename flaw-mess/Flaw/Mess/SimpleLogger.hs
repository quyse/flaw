{-|
Module: Flaw.Mess.SimpleLogger
Description: Simple logger for Mess dispatcher, writing text output to stdout.
License: MIT
-}

module Flaw.Mess.SimpleLogger
	( SimpleLogger(..)
	) where

import Control.Exception
import Data.Time

import Flaw.Mess
import Flaw.Mess.Dispatcher

data SimpleLogger = SimpleLogger

showHash :: ExpressionHash -> String
showHash (ExpressionHash d) = take 10 $ show d

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale rfc822DateFormat -- (iso8601DateFormat Nothing)

instance DispatcherLogger SimpleLogger where
	dispatchLogStart SimpleLogger he = do
		t <- getCurrentTime
		putStrLn $ showTime t ++ ": starting " ++ showHash (hashedExpressionHash he)
	dispatchLogRequest SimpleLogger mhef het = do
		t <- getCurrentTime
		let requester = case mhef of
			Just hef -> showHash (hashedExpressionHash hef) ++ " requested "
			Nothing -> "requested "
		putStrLn $ showTime t ++ ": " ++ requester ++ showHash (hashedExpressionHash het)
	dispatchLogSuccess SimpleLogger he = do
		t <- getCurrentTime
		putStrLn $ showTime t ++ ": finished " ++ showHash (hashedExpressionHash he)
	dispatchLogError SimpleLogger he (SomeException e) = do
		t <- getCurrentTime
		putStrLn $ showTime t ++ ": failed " ++ showHash (hashedExpressionHash he) ++ ": " ++ show e
