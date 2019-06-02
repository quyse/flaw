{-|
Module: Flaw.Network
Description: Network abstraction.
License: MIT
-}

module Flaw.Network
  ( Socket(..)
  , SocketException(..)
  ) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString as B

class Socket s where
  readSocket :: s -> STM B.ByteString
  writeSocket :: s -> B.ByteString -> STM ()
  closeSocket :: s -> STM ()

data SocketException
  = SocketEndException
  | SocketErrorException
  deriving Show

instance Exception SocketException
