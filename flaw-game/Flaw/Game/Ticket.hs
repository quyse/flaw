{-|
Module: Flaw.Game.Ticket
Description: Tickets - signed pieces of data authenticating clients.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Flaw.Game.Ticket
	( Ticket(..)
	, AEADTicket(..)
	, initAEADTicketWitness
	) where

import Crypto.Cipher.Types
import Crypto.Error
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import GHC.Generics(Generic)

-- | Abstract ticket class.
class Ticket t where
	data TicketWitness t :: *
	createTicket :: S.Serialize a => TicketWitness t -> a -> t a
	verifyTicket :: S.Serialize a => TicketWitness t -> t a -> Maybe a

data AEADTicket c a = AEADTicket !B.ByteString !B.ByteString deriving (Generic, Show)

instance S.Serialize (AEADTicket c a)

instance BlockCipher c => Ticket (AEADTicket c) where
	data TicketWitness (AEADTicket c) = AEADTicketWitness (AEAD c)
	createTicket (AEADTicketWitness aead) a = AEADTicket cipherText $ BA.convert authTag where
		(cipherText, aead2) = aeadEncrypt aead (S.encode a)
		authTag = aeadFinalize aead2 16
	verifyTicket (AEADTicketWitness aead) (AEADTicket cipherText inputAuthTag) = result where
		(plainText, aead2) = aeadDecrypt aead cipherText
		authTag = aeadFinalize aead2 16
		result = if BA.convert authTag == inputAuthTag then decodedPlainText else Nothing
		decodedPlainText = case S.decode plainText of
			Right r -> Just r
			Left _e -> Nothing

initAEADTicketWitness :: BlockCipher c => B.ByteString -> B.ByteString -> TicketWitness (AEADTicket c)
initAEADTicketWitness key iv = AEADTicketWitness $ throwCryptoError $ do
	cipher <- cipherInit key
	aeadInit AEAD_GCM cipher iv
