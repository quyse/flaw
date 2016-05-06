{-|
Module: Flaw.Network.MessageSocket
Description: Serializing wrapper over socket.
License: MIT
-}

{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Flaw.Network.MessageSocket
	( MessageSocket(..)
	, readMessageSocket
	, writeMessageSocket
	) where

import Control.Concurrent.STM
import qualified Data.Serialize as S

import Flaw.Network

data MessageSocket i o where
	MessageSocket :: Socket s => s -> MessageSocket i o

readMessageSocket :: S.Serialize i => MessageSocket i o -> STM i
readMessageSocket (MessageSocket socket) = do
	eitherValue <- S.decode <$> readSocket socket
	return $ case eitherValue of
		Right value -> value
		Left err -> error err

writeMessageSocket :: S.Serialize o => MessageSocket i o -> o -> STM ()
writeMessageSocket (MessageSocket socket) = writeSocket socket . S.encode
