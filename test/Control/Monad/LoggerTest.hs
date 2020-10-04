{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Control.Monad.LoggerTest where

import Control.Monad.Logger
import Control.Monad.Writer.Strict (Writer, execWriter)
import Data.Function (on)
import Prelude hiding (log)

(==*) :: (Eq w, Show w) => Writer [w] x -> Writer [w] x -> Bool
(==*) = (==) `on` execWriter

infix 4 ==*

prop_identity :: [Int] -> Bool
prop_identity xs = m `logging` id ==* m
  where
    m :: forall m. MonadLogger Int m => m ()
    m = mapM_ log xs

prop_composition :: [Int] -> (Int -> Int) -> (Int -> String) -> Bool
prop_composition xs f g = m `logging` f `logging` g ==* m `logging` (g . f)
  where
    m :: forall m. MonadLogger Int m => m ()
    m = mapM_ log xs

prop_homomorphism :: Int -> (Int -> String) -> Bool
prop_homomorphism x f = log x `logging` f ==* log (f x)
