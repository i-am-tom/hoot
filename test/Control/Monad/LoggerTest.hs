{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.LoggerTest where

import Control.Monad.Logger
import Data.Function (on)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Kind (Type)
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Prelude hiding (log)
import System.IO.Unsafe (unsafePerformIO)

-- | A newtype wrapper that we can specialise to give us @Eq@ and @Show@.
newtype PureT (w :: Type) (m :: Type -> Type) (x :: Type)
  = PureT { runPureT :: LoggerT w m x }
  deriving newtype (Functor, Applicative, Monad, MonadLogger)

-- | Convenience synonym for @PureT@ specialised to @IO@.
type Pure (w :: Type) (x :: Type)
  = PureT w IO x

instance (w ~ String, m ~ IO, Eq x)
    => Eq (PureT w m x) where
  (==) = (==) `on` runPure

instance (Show w, m ~ IO, Show x)
    => Show (PureT w m x) where
  show = show . runPure . flip logging show

-- | Extract the end result and accompanying logs from a @Pure@ value by
-- sinking all logs into a list.
runPure :: Pure String x -> (x, [ String ])
runPure (PureT action) = unsafePerformIO do
  logs <- newIORef []

  let sink :: Sink IO String
      sink = Sink (modifyIORef logs . (++) . pure)

  output <- readIORef logs
  result <- runLoggerT action sink

  pure ( result, output )

-- | Using the above specialisations, we can now create a generator for 'Pure'
-- values, which we can use in our property tests. Specifically, we generate a
-- list of integers (our logs), and 'log' each of them in turn to fill it.
logger :: Gen x -> Gen (Pure x ())
logger gen = fmap (PureT . mapM_ log) do
  Gen.list (Range.linear 0 10) gen

-- | Generator for integers.
ints :: Gen Int
ints = Gen.int (Range.linear 0 1000)

-- | Generator for strings.
strings :: Gen String
strings = Gen.list (Range.linear 0 10) Gen.alphaNum

------------------------------------------------------------

hprop_PureT_identity :: Property
hprop_PureT_identity = property do
  m <- forAll (logger strings)
  m `logging` id === m

hprop_PureT_composition :: Property
hprop_PureT_composition = property do
  m <- forAll (logger ints)

  m `logging` (+ 1) `logging` show
    === m `logging` (show . (+ 1))

hprop_PureT_homomorphism :: Property
hprop_PureT_homomorphism = property do
  x <- forAll (Gen.int (Range.linear 0 1000))
  log @PureT x `logging` show === log (show x)
