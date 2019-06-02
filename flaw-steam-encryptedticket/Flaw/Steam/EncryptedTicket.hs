{-|
Module: Flaw.Steam.EncryptedTicket
Description: Steam Encrypted Ticket API.
License: MIT
-}

module Flaw.Steam.EncryptedTicket
  ( SteamEncryptedTicketKey(..)
  , steamVerifyEncryptedTicket
  , SteamEncryptedTicketException(..)
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Steam

-- | Key for deciphering encrypted tickets.
newtype SteamEncryptedTicketKey = SteamEncryptedTicketKey B.ByteString

steamVerifyEncryptedTicket :: SteamAppId -> SteamEncryptedTicketKey -> NominalDiffTime -> B.ByteString -> IO SteamId
steamVerifyEncryptedTicket (SteamAppId appId) (SteamEncryptedTicketKey key) timeout encryptedTicket =
  -- decrypt ticket
  B.unsafeUseAsCStringLen encryptedTicket $ \(encryptedTicketPtr, encryptedTicketLen) ->
    allocaBytes (fromIntegral decryptedTicketBufLen) $ \decryptedTicketPtr -> do
      decryptedTicketLen <- with decryptedTicketBufLen $ \decryptedTicketLenPtr -> do
        ok <- B.unsafeUseAsCStringLen key $ \(keyPtr, keyLen) ->
          c_BDecryptTicket encryptedTicketPtr (fromIntegral encryptedTicketLen) decryptedTicketPtr decryptedTicketLenPtr keyPtr (fromIntegral keyLen)
        unless ok $ throwIO SteamEncryptedTicketException_DecryptionFailed
        peek decryptedTicketLenPtr
      -- check that ticket is for specified appid
      do
        ok <- c_BIsTicketForApp decryptedTicketPtr decryptedTicketLen appId
        unless ok $ throwIO SteamEncryptedTicketException_WrongAppId
      -- check that ticket is not timed out
      do
        issueTime <- posixSecondsToUTCTime . fromIntegral <$> c_GetTicketIssueTime decryptedTicketPtr decryptedTicketLen
        currentTime <- getCurrentTime
        unless (currentTime `diffUTCTime` issueTime < timeout) $ throwIO SteamEncryptedTicketException_TicketTimedOut
      -- get steam id
      alloca $ \steamIdPtr -> do
        c_GetTicketSteamID decryptedTicketPtr decryptedTicketLen steamIdPtr
        SteamId <$> peek steamIdPtr
  where
    decryptedTicketBufLen = 1024

data SteamEncryptedTicketException
  = SteamEncryptedTicketException_DecryptionFailed
  | SteamEncryptedTicketException_WrongAppId
  | SteamEncryptedTicketException_TicketTimedOut
  deriving Show
instance Exception SteamEncryptedTicketException

foreign import ccall safe "SteamEncryptedAppTicket_BDecryptTicket" c_BDecryptTicket :: Ptr CChar -> Word32 -> Ptr CChar -> Ptr Word32 -> Ptr CChar -> CInt -> IO Bool
foreign import ccall safe "SteamEncryptedAppTicket_BIsTicketForApp" c_BIsTicketForApp :: Ptr CChar -> Word32 -> Word32 -> IO Bool
foreign import ccall safe "SteamEncryptedAppTicket_GetTicketIssueTime" c_GetTicketIssueTime :: Ptr CChar -> Word32 -> IO Word32
foreign import ccall safe "SteamEncryptedAppTicket_GetTicketSteamID" c_GetTicketSteamID :: Ptr CChar -> Word32 -> Ptr Word64 -> IO ()
