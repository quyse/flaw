{-|
Module: Flaw.Mess
Description: Building with pure functions.
License: MIT
-}

{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}

module Flaw.Mess
	( Value(..)
	, IsValue(..)
	, Expression(..)
	, HashedExpression(..)
	, ExpressionHash(..)
	, hashExpression
	, unhashExpression
	, DispatchMonad(..)
	, MessT()
	, calc
	, closure
	) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Crypto.Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.Hashable(Hashable(hashWithSalt))
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S

-- | Untyped value containing data, calculation or function.
data Value m
	= DataValue B.ByteString
	| CalcValue (MessT m (Value m))
	| FuncValue (Value m -> MessT m (Value m))
	| ClosureValue (Value m -> MessT m (Value m)) (MessT m (Value m))
	| ReturnValue (Value m)

-- | Class of things capable of being packed to Value.
-- Has default instance for all types supporting S.Serialize.
class IsValue a m where
	-- | Encode thing to Value.
	toValue :: a -> Value m
	default toValue :: S.Serialize a => a -> Value m
	toValue = DataValue . S.encode
	{-# INLINABLE toValue #-}

	-- | Decode thing from Value.
	fromValue :: Value m -> a
	default fromValue :: S.Serialize a => Value m -> a
	fromValue v = case v of
		DataValue b -> case S.decode b of
			Right d -> d
			Left e -> error $ "fromValue data decoding error: " ++ e
		_ -> error "fromValue expected data"
	{-# INLINABLE fromValue #-}

-- | Instance for Value.
instance IsValue (Value m) m where
	toValue = id
	{-# INLINABLE toValue #-}
	fromValue = id
	{-# INLINABLE fromValue #-}

-- | Instance for functions.
instance (IsValue a m, IsValue b m, Monad m) => IsValue (a -> MessT m b) m where
	toValue f = FuncValue $ liftM toValue . f . fromValue
	{-# INLINABLE toValue #-}
	fromValue v = case v of
		FuncValue f -> liftM fromValue . f . toValue
		_ -> error "fromValue expected function"
	{-# INLINABLE fromValue #-}

-- | Instance for calculations.
-- This hash to be before instances for tuples and list, as they all match IsValue (m a) m
-- (that's why IncoherentInstances is enabled).
instance (IsValue a m, Monad m) => IsValue (MessT m a) m where
	toValue c = CalcValue $ liftM toValue c
	{-# INLINABLE toValue #-}
	fromValue v = case v of
		CalcValue c -> liftM fromValue c
		_ -> error "fromValue expected calculation"
	{-# INLINABLE fromValue #-}

-- Some data instances of IsValue.
instance IsValue () m
instance IsValue Bool m
instance IsValue Char m
instance IsValue Int m
instance IsValue Float m
instance IsValue Double m
instance (S.Serialize a, IsValue a m) => IsValue [a] m
instance (S.Serialize a, S.Serialize b, IsValue a m, IsValue b m) => IsValue (a, b) m
instance (S.Serialize a, S.Serialize b, S.Serialize c, IsValue a m, IsValue b m, IsValue c m) => IsValue (a, b, c) m

-- | Expression describes function or data to calculate.
data Expression
	-- | Root of all expressions.
	= ExpressionRoot
	-- | Pure data; type is value.
	| ExpressionPure
		{ expressionData :: B.ByteString
		}
	-- | First closure from calculation; type is closure.
	| ExpressionStart
		{ expressionParent :: Expression
		}
	-- | Next closure from calculation; type is closure.
	| ExpressionStep
		{ expressionParent :: Expression
		}
	-- | Application of closure to argument; type is calculation.
	| ExpressionApplication
		{ expressionClosure :: Expression
		, expressionArgument :: Expression
		}
	-- | Evaluation of calculation; type is value.
	| ExpressionEvaluation
		{ expressionParent :: Expression
		}

-- | Analog of expression, but with hashes.
data HashedExpression
	= HashedExpressionRoot
		{ hashedExpressionHash :: ExpressionHash
		}
	| HashedExpressionPure
		{ hashedExpressionHash :: ExpressionHash
		, hashedExpressionData :: B.ByteString
		}
	| HashedExpressionStart
		{ hashedExpressionHash :: ExpressionHash
		, hashedExpressionParent :: HashedExpression
		}
	| HashedExpressionStep
		{ hashedExpressionHash :: ExpressionHash
		, hashedExpressionParent :: HashedExpression
		}
	| HashedExpressionApplication
		{ hashedExpressionHash :: ExpressionHash
		, hashedExpressionClosure :: HashedExpression
		, hashedExpressionArgument :: HashedExpression
		}
	| HashedExpressionEvaluation
		{ hashedExpressionHash :: ExpressionHash
		, hashedExpressionParent :: HashedExpression
		}
	deriving Show

newtype ExpressionHash = ExpressionHash (Digest SHA256) deriving (Eq, Ord, Show)

instance Hashable ExpressionHash where
	hashWithSalt s (ExpressionHash d) = hashWithSalt s (BA.convert d :: B.ByteString)

hashRootHash :: ExpressionHash
hashRootHash = ExpressionHash $ hash $ S.runPut $ S.putWord8 0

putExpressionHash :: ExpressionHash -> S.Put
putExpressionHash (ExpressionHash d) = S.put (BA.convert d :: B.ByteString)

appendExpressionStart :: HashedExpression -> HashedExpression
appendExpressionStart phe = HashedExpressionStart h phe where
	h = ExpressionHash $ hash $ S.runPut $ do
		putExpressionHash $ hashedExpressionHash phe
		S.putWord8 2

appendExpressionStep :: HashedExpression -> HashedExpression
appendExpressionStep phe = HashedExpressionStep h phe where
	h = ExpressionHash $ hash $ S.runPut $ do
		putExpressionHash $ hashedExpressionHash phe
		S.putWord8 3

appendExpressionEvaluation :: HashedExpression -> HashedExpression
appendExpressionEvaluation phe = HashedExpressionEvaluation h phe where
	h = ExpressionHash $ hash $ S.runPut $ do
		putExpressionHash $ hashedExpressionHash phe
		S.putWord8 5

hashExpression :: Expression -> HashedExpression
hashExpression e = case e of
	ExpressionRoot -> HashedExpressionRoot hashRootHash
	ExpressionPure d -> let
		h = ExpressionHash $ hash $ S.runPut $ do
			S.put d
			S.putWord8 1
		in HashedExpressionPure h d
	ExpressionStart pe -> appendExpressionStart $ hashExpression pe
	ExpressionStep pe -> appendExpressionStep $ hashExpression pe
	ExpressionApplication fe ae -> let
		fhe = hashExpression fe
		ahe = hashExpression ae
		h = ExpressionHash $ hash $ S.runPut $ do
			putExpressionHash $ hashedExpressionHash fhe
			putExpressionHash $ hashedExpressionHash ahe
			S.putWord8 4
		in HashedExpressionApplication h fhe ahe
	ExpressionEvaluation pe -> appendExpressionEvaluation $ hashExpression pe

-- | Convert hashed expression into expression (very lazily).
unhashExpression :: HashedExpression -> Expression
unhashExpression = snd . u HM.empty where
	u hm he = case HM.lookup (hashedExpressionHash he) hm of
		Just e -> (hm, e)
		Nothing -> case he of
			HashedExpressionRoot h -> let
				e = ExpressionRoot
				in (HM.insert h e hm, e)
			HashedExpressionPure h d -> let
				e = ExpressionPure d
				in (HM.insert h e hm, e)
			HashedExpressionStart h p -> let
				e = ExpressionStart pe
				(phm, pe) = u (HM.insert h e hm) p
				in (phm, e)
			HashedExpressionStep h p -> let
				e = ExpressionStep pe
				(phm, pe) = u (HM.insert h e hm) p
				in (phm, e)
			HashedExpressionApplication h f a -> let
				e = ExpressionApplication fe ae
				(fhm, fe) = u (HM.insert h e hm) f
				(ahm, ae) = u fhm a
				in (ahm, e)
			HashedExpressionEvaluation h p -> let
				e = ExpressionEvaluation pe
				(phm, pe) = u (HM.insert h e hm) p
				in (phm, e)

class Monad m => DispatchMonad m where
	-- | Get root value.
	dispatchRootValue :: m (Value m)
	-- | For each input pair get a value from cache if it's there,
	-- otherwise perform calculation and put result into cache.
	-- Fully reentrant. Performs calculation of values in parallel,
	-- and returns when everything is calculated.
	dispatchCalc :: [(HashedExpression, m (Value m))] -> m [Value m]

newtype MessT m a = MessT (m (Mess m a))

data Mess m a
	= MessClosure (Value m -> MessT m (Value m)) (MessT m a)
	| MessFinal a

instance Monad m => Functor (MessT m) where
	fmap f (MessT m) = MessT $ do
		r <- m
		return $ case r of
			MessClosure c t -> MessClosure c $ fmap f t
			MessFinal a -> MessFinal $ f a
	{-# INLINE fmap #-}

instance Monad m => Applicative (MessT m) where
	pure = MessT . return . MessFinal
	{-# INLINE pure #-}
	mf <*> ma = do
		f <- mf
		a <- ma
		return $ f a
	{-# INLINE (<*>) #-}

instance Monad m => Monad (MessT m) where
	return = MessT . return . MessFinal
	{-# INLINE return #-}
	MessT m >>= f = MessT $ do
		r <- m
		case r of
			MessClosure c t -> return $ MessClosure c $ t >>= f
			MessFinal a -> do
				let MessT q = f a
				q
	{-# INLINE (>>=) #-}

instance MonadTrans MessT where
	lift a = MessT $ liftM MessFinal a
	{-# INLINE lift #-}

instance MonadIO m => MonadIO (MessT m) where
	liftIO = lift . liftIO
	{-# INLINE liftIO #-}

calc :: DispatchMonad m => [HashedExpression] -> m [Value m]
calc = dispatchCalc . map z where
	z he = (he, g he)
	g he = case he of
		HashedExpressionRoot {} -> dispatchRootValue
		HashedExpressionPure
			{ hashedExpressionData = d
			} -> return $ DataValue d
		HashedExpressionStart
			{ hashedExpressionParent = phe
			} -> do
			[p] <- calc [phe]
			case p of
				CalcValue (MessT m) -> do
					r <- m
					case r of
						MessClosure c t -> return $ ClosureValue c t
						MessFinal a -> return $ ReturnValue a
				_ -> error "calc expected parent calculation for start"
		HashedExpressionStep
			{ hashedExpressionParent = phe
			} -> do
			[p] <- calc [phe]
			case p of
				ClosureValue _c (MessT m) -> do
					r <- m
					return $ case r of
						MessClosure c t -> ClosureValue c t
						MessFinal a -> ReturnValue $ toValue a
				_ -> error "calc expected parent closure for step"
		HashedExpressionApplication
			{ hashedExpressionClosure = fhe
			, hashedExpressionArgument = ahe
			} -> do
			[f, a] <- calc [fhe, ahe]
			case f of
				ClosureValue c _t -> do
					return $ toValue $ c a
				_ -> error "calc expected closure for application"
		HashedExpressionEvaluation
			{ hashedExpressionParent = phe
			} -> do
			[p] <- calc [phe]
			case p of
				CalcValue _ -> do
					[r] <- calc [appendExpressionEvaluation $ appendExpressionStart phe]
					return r
				ClosureValue _ _ -> do
					[r] <- calc [appendExpressionEvaluation $ appendExpressionStep phe]
					return r
				ReturnValue a -> return a
				_ -> error "calc expected parent calculation, closure or return value for evaluation"
{-# INLINABLE calc #-}

closure :: (IsValue a m, IsValue b m, Monad m) => (a -> MessT m b) -> MessT m (a -> MessT m b)
closure f = MessT $ return $ MessClosure (liftM toValue . f . fromValue) $ MessT $ return $ MessFinal f
{-# INLINABLE closure #-}
