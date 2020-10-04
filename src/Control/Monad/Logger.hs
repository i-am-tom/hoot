{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Kind (Type)
import Prelude hiding (log)

-- | The interface for 'Logger' looks very similar to 'MonadWriter', but it
-- doesn't require @Monoid w@. Conceptually, we can think of @MonadLogger w m@
-- as being morally equivalent to @MonadWriter [w] m@; we're just logging each
-- action in turn.
--
-- However, we typically don't "collect" logs; we just print them out,
-- according to the provided sinking function.
class Monad m => MonadLogger (w :: Type) (m :: Type -> Type) | m -> w where

  -- | Log a message with the sinking function.
  log :: w -> m ()

-- | In our code, we should try not to concretise the transformer stack
-- anywhere but at the application level. If we simply stick to constraints, we
-- can use this function to map the logs of an inner function to an outer one.
logging :: MonadLogger o m => LoggerT i m x -> (i -> o) -> m x
logging inner f = runLoggerT inner (log . f)

-- | A transformer for logging values with an action in the context of the
-- inner @m@ monad. Intuitively quite similar to 'Control.Monad.Trans.WriterT',
-- except that the logged values aren't necessarily stored; typically, they're
-- handled immediately, e.g. by printing them to a terminal.
newtype LoggerT (w :: Type) (m :: Type -> Type) (x :: Type)
  = LoggerT { runLoggerT :: (w -> m ()) -> m x }
  deriving (Functor, Applicative, Monad)
    via (ReaderT (ReaderT w m ()) m)

instance (Monad m, w ~ w')
    => MonadLogger w (LoggerT w' m) where
  log :: Monad m => w -> LoggerT w m ()
  log w = LoggerT \sink -> sink w

instance (Monad m, w ~ w')
    => MonadLogger w' (CPS.WriterT [w] m) where
  log :: Monad m => w -> CPS.WriterT [w] m ()
  log = CPS.tell . pure

instance (Monad m, w ~ w')
    => MonadLogger w' (Lazy.WriterT [w] m) where
  log :: Monad m => w -> Lazy.WriterT [w] m ()
  log = Lazy.tell . pure

instance (Monad m, w ~ w')
    => MonadLogger w' (Strict.WriterT [w] m) where
  log :: Monad m => w -> Strict.WriterT [w] m ()
  log = Strict.tell . pure
