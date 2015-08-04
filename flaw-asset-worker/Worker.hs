{-|
Module: Main
Description: Worker process.
License: MIT
-}

module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S

import Flaw.Asset.Task

main :: IO ()
main = do
	maybeWrappedTaskInput <- liftM (S.runGetLazy $ getTaskInput taskTags) BL.getContents
	(BL.putStr . S.runPutLazy) =<< case maybeWrappedTaskInput of
		Right (Just (WrappedTaskInput taskInput)) -> do
			taskOutput <- runTask taskInput
			return $ do
				S.put True
				S.put taskOutput
		_ -> return $ S.put False

taskTags :: [TaskTag]
taskTags = []
