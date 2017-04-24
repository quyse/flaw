{-|
Module: Flaw.Script
Description: Scripting in Haskell.
License: MIT
-}

{-# LANGUAGE GADTs, ViewPatterns #-}

module Flaw.Script
	( Interpreter()
	, newInterpreter
	, interpret
	, I.InterpreterError(..)
	) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import qualified Data.Text as T
import Data.Typeable
import qualified Language.Haskell.Interpreter as I

import Flaw.Book
import Flaw.Flow

newtype Interpreter = Interpreter
	{ interpreterTaskQueue :: TQueue Task
	}

data Task where
	Task :: Typeable a =>
		{ taskModules :: [T.Text]
		, taskExpression :: !T.Text
		, taskResultVar :: {-# UNPACK #-} !(TMVar (Either I.InterpreterError a))
		} -> Task

newInterpreter :: [T.Text] -> IO (Interpreter, IO ())
newInterpreter (map T.unpack -> searchPaths) = withSpecialBook $ \bk -> do
	taskQueue <- newTQueueIO
	activeVar <- newTVarIO True
	closedVar <- newEmptyMVar

	book bk $ forkFlow $ flip finally (putMVar closedVar ()) $ void $ I.runInterpreter $ do
		-- extensions set once, since they are not reset with `reset`
		I.set
			[ I.languageExtensions I.:= defaultExtensions
			, I.searchPath I.:= searchPaths
			]

		let step = do
			-- wait for task
			maybeTask <- I.liftIO $ atomically $ let
				getTask = Just <$> readTQueue taskQueue
				getUnactive = do
					active <- readTVar activeVar
					when active retry
					return Nothing
				in getTask `orElse` getUnactive

			case maybeTask of
				-- if there's task
				Just Task
					{ taskModules = map T.unpack -> modules
					, taskExpression = T.unpack -> expression
					, taskResultVar = resultVar
					} -> do
					-- handle task
					handle (I.liftIO . atomically . putTMVar resultVar . Left) $ do
						-- load modules
						I.loadModules modules
						I.setTopLevelModules modules
						let
							run :: (I.MonadInterpreter m, Typeable a) => TMVar (Either I.InterpreterError a) -> a -> m a
							run _ witness = I.interpret expression witness
						I.liftIO . atomically . putTMVar resultVar . Right =<< run resultVar undefined
					-- repeat
					step
				-- otherwise we're shutting down
				Nothing -> return ()
		step

	-- graceful shutdown
	book bk $ return ((), do
		atomically $ writeTVar activeVar False
		takeMVar closedVar
		)

	return Interpreter
		{ interpreterTaskQueue = taskQueue
		}

interpret :: Typeable a => Interpreter -> [T.Text] -> T.Text -> IO a
interpret Interpreter
	{ interpreterTaskQueue = taskQueue
	} modules expression = do
	resultVar <- newEmptyTMVarIO
	atomically $ do
		writeTQueue taskQueue Task
			{ taskModules = modules
			, taskExpression = expression
			, taskResultVar = resultVar
			}
	eitherResult <- atomically $ takeTMVar resultVar
	case eitherResult of
		Left e -> throwM e
		Right result -> return result

-- | Extensions considered harmless and useful to be enabled automatically.
defaultExtensions :: [I.Extension]
defaultExtensions =
	[
	-- , I.OverlappingInstances -- deprecated
	-- , I.UndecidableInstances -- dangerous
	-- , I.IncoherentInstances -- deprecated
	-- , I.DoRec -- ?
	  I.RecursiveDo -- harmless
	, I.ParallelListComp -- harmless
	, I.MultiParamTypeClasses -- needed
	, I.NoMonomorphismRestriction -- useful in scripts
	, I.FunctionalDependencies -- needed
	-- , I.Rank2Types -- obsolete, replaced with RankNTypes
	, I.RankNTypes -- needed
	-- , I.PolymorphicComponents -- obsolete, replaced with RankNTypes
	, I.ExistentialQuantification -- needed
	-- , I.ScopedTypeVariables -- usually unexpected
	, I.PatternSignatures -- needed
	, I.ImplicitParams -- harmless
	, I.FlexibleContexts -- needed
	, I.FlexibleInstances -- needed
	, I.EmptyDataDecls -- needed
	, I.NoCPP -- not sure, try to refrain from for now
	, I.KindSignatures -- harmless, also implied by TypeFamilies
	, I.BangPatterns -- harmless
	, I.TypeSynonymInstances -- implied by FlexibleInstances
	, I.TemplateHaskell -- needed
	, I.NoForeignFunctionInterface -- not needed in scripts
	, I.Arrows -- harmless
	, I.Generics -- ?
	-- , I.ImplicitPrelude -- enabled by default
	, I.NamedFieldPuns -- harmless
	, I.PatternGuards -- harmless
	, I.GeneralizedNewtypeDeriving -- needed
	-- , I.ExtensibleRecords -- ?
	-- , I.RestrictedTypeSynonyms -- ?
	-- , I.HereDocuments -- ?
	, I.NoMagicHash -- not needed
	, I.TypeFamilies -- needed
	, I.StandaloneDeriving -- needed
	, I.UnicodeSyntax -- harmless
	-- , I.UnliftedFFITypes -- FFI is disabled
	-- , I.InterruptibleFFI -- FFI is disabled
	-- , I.CApiFFI -- FFI is disabled
	, I.NoLiberalTypeSynonyms -- dangerous
	-- , I.TypeOperators -- not needed
	, I.RecordWildCards -- harmless
	-- , I.RecordPuns -- ?
	-- , I.DisambiguateRecordFields -- implied by DuplicateRecordFields
	-- , I.TraditionalRecordSyntax -- enabled by default
	, I.OverloadedStrings -- needed
	, I.GADTs -- needed
	-- , I.GADTSyntax -- implied by GADTs
	-- , I.MonoPatBinds -- ?
	-- , I.RelaxedPolyRec -- fed by ScopedTypeVariables
	-- , I.ExtendedDefaultRules -- ?
	-- , I.UnboxedTuples -- not needed
	, I.DeriveDataTypeable -- needed
	, I.DeriveGeneric -- needed
	, I.DefaultSignatures -- needed
	, I.InstanceSigs -- harmless
	-- , I.ConstrainedClassMethods -- implied by MultiParamTypeClasses
	-- , I.PackageImports -- generally not recommended
	-- , I.ImpredicativeTypes -- dangerous
	-- , I.NewQualifiedOperators -- ?
	-- , I.PostfixOperators -- seem to be harmless, but surprising
	, I.QuasiQuotes -- needed
	, I.TransformListComp -- harmless
	, I.MonadComprehensions -- harmless
	, I.ViewPatterns -- needed
	-- , I.XmlSyntax -- ?
	-- , I.RegularPatterns -- ?
	, I.TupleSections -- harmless
	-- , I.GHCForeignImportPrim -- ?
	-- , I.NPlusKPatterns -- ?
	-- , I.DoAndIfThenElse -- ?
	, I.MultiWayIf -- harmless
	, I.LambdaCase -- harmless
	, I.NoRebindableSyntax -- dangerous
	, I.ExplicitForAll -- harmless
	-- , I.DatatypeContexts -- deprecated
	-- , I.MonoLocalBinds -- implied by GADTs, TypeFamilies
	-- , I.DeriveFunctor -- not needed?
	-- , I.DeriveTraversable -- not needed?
	-- , I.DeriveFoldable -- not needed
	-- , I.NondecreasingIndentation -- ?
	-- , I.SafeImports -- ?
	-- , I.Safe -- not declaring safety
	-- , I.Trustworthy -- not declaring safety
	-- , I.Unsafe -- not declaring safety
	, I.ConstraintKinds -- needed
	-- , I.PolyKinds -- no kind stuff yet
	-- , I.DataKinds -- no kind stuff yet
	-- , I.ParallelArrays -- ?
	, I.RoleAnnotations -- harmless
	, I.OverloadedLists -- needed
	, I.EmptyCase -- harmless
	-- , I.AutoDeriveTypeable -- ?
	, I.NegativeLiterals -- useful
	, I.BinaryLiterals -- harmless
	, I.NumDecimals -- harmless
	-- , I.NullaryTypeClasses -- replaced by MultiParamTypeClasses
	-- , I.ExplicitNamespaces -- implied by TypeFamilies
	-- , I.AllowAmbiguousTypes -- dangerous
	-- , I.JavaScriptFFI -- GHCJS only
	, I.PatternSynonyms -- needed
	, I.PartialTypeSignatures -- useful in scripts
	, I.NamedWildCards -- harmless
	-- , I.DeriveAnyClass -- dangerous
	-- , I.DeriveLift -- not needed?
	, I.StaticPointers -- harmless
	-- , I.StrictData -- dangerous
	-- , I.Strict -- dangerous
	-- , I.ApplicativeDo -- dangerous
	, I.DuplicateRecordFields -- useful
	, I.TypeApplications -- harmless
	, I.TypeInType -- no kind stuff yet
	-- , I.UndecidableSuperClasses -- dangerous
	, I.MonadFailDesugaring -- needed to check that it will work with future GHC versions
	-- , I.TemplateHaskellQuotes -- implied by TemplateHaskell
	-- , I.OverloadedLabels -- harmless, but not needed
	]
