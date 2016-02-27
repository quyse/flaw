{-|
Module: Flaw.Game.State.Lockstep
Description: Deterministic game state using lockstep.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Game.State.Lockstep
	( Lockstep(..)
	) where

class Lockstep a where
	-- | State of the logic.
	data LockstepState a :: *
	-- | Intent is a client's wish what to do.
	-- Usually it's a combined set of commands sent by clients.
	data LockstepIntent a :: *
	-- | Calculate new state.
	-- Returns result (next) state and function to rebase intent to the next state.
	runLockstep
		:: LockstepState a -- ^ Current state.
		-> LockstepIntent a -- ^ Intent to apply on current state.
		-> (LockstepState a, LockstepIntent a -> LockstepIntent a)
