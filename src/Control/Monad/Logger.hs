{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Control.Monad.Logger
Description : An interface and implementation for logging.
Copyright   : (c) Tom Harding, 2020
License     : MIT
Maintainer  : i.am.tom.harding@gmail.com
Stability   : experimental
-}
module Control.Monad.Logger where

import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Contravariant (Contravariant (..), Op (..))
import Data.Kind (Type)
import Data.Monoid (Ap (..))

-- | A class for monads indexed by the type of the logs that they can handle.
-- They should be entirely polymorphic in the logging type, as the function of
-- an implementer is to thread these logs through to parent call sites.
--
-- Laws:
--
-- prop> m `logging` id == m
--
-- prop> (m `logging` f) `logging` g == m `logging` (g . f)
--
-- prop> log x `logging` f == log (f x)
--
-- TODO: this doesn't compose super well because we can't just parameterise the
-- type of @w@. If we did, we couldn't implement 'logging'. There probably /is/
-- a neater abstraction, but it needs further research.
class (forall (w :: Type) (m :: Type -> Type). Monad m => Monad (t w m))
    => MonadLogger (t :: Type -> (Type -> Type) -> Type -> Type) where

  -- | Create a new logging value. For the sake of @do@-notation, all values
  -- logged within a function should be of the same type.
  log :: Monad m => w -> t w m ()

  -- | Transform the logs within a given action. Typically, this is used to
  -- absorb the logs of a child function into the logs of a parent function.
  logging :: Monad m => t w m x -> (w -> w') -> t w' m x

-- | A sink is a function that "logs" a given value according to some monadic
-- context. In practice, @m@ is going to be some 'IO'-capable monad to perform
-- the action of logging to the console, or database, or anywhere else.
newtype Sink (m :: Type -> Type) (x :: Type)
  = Sink { withSink :: x -> m () }
  deriving (Semigroup, Monoid) via (Ap (ReaderT x m) ())
  deriving Contravariant via (Op (m ()))

-- | A monad for logging values using an action in the @m@ context. Internally,
-- this is a 'Sink' action, and probably involves some 'IO'.
newtype LoggerT (w :: Type) (m :: Type -> Type) (x :: Type)
  = LoggerT { runLoggerT :: Sink m w -> m x }
  deriving (Functor, Applicative, Monad)
    via (ReaderT (Sink m w) m)

instance MonadLogger LoggerT where
  log :: Monad m => w -> LoggerT w m ()
  log = LoggerT . flip withSink

  logging :: LoggerT w m x -> (w -> w') -> LoggerT w' m x
  logging (LoggerT k) expand = LoggerT (k . contramap expand)
