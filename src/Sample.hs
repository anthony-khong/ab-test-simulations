{-# LANGUAGE MultiParamTypeClasses #-}

module Sample where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Random               as DR
import           Data.Random.Source.PureMT
import           Data.Word

sample :: (Num t, DR.Distribution d t) => Word64 -> d t -> [t]
sample seed dist = evalState (iterateM $ DR.sample dist) (pureMT seed)

iterateM :: (Applicative m) => m a -> m [a]
iterateM f = liftA2 (:) f (iterateM f)
