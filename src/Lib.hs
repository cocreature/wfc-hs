module Lib
    (
    ) where

import           Protolude hiding ((<>))

import           Control.Monad.Random.Class (MonadRandom, getRandom)
import           Data.List (elemIndex)
import           Data.Massiv.Array (Array, B, D, Ix1, Ix2, Mutable, Source, U)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv
import qualified Data.Massiv.Array.Unsafe as Massiv
import           Data.Semigroup

observe :: MonadRandom m => Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> m ObservationResult
observe stationary wave = do
  r <- minEntropy stationary wave
  case r of
    Contradiction -> pure ObsContradiction
    Zero -> pure (FinalResult (Massiv.compute (Massiv.map findAssignment wave)))
    Entropy i -> do
      val <- sampleArray (Massiv.zipWith (\possible count -> if possible then count else 0) (wave Massiv.! i) stationary)
      let newVals = Massiv.makeArray Massiv.Seq (Massiv.size stationary) (val==)
      -- TODO we shouldn’t copy the whole array here
          wave' = runST $ do
            w <- Massiv.thaw wave
            Massiv.write' w i newVals
            Massiv.unsafeFreeze Massiv.Seq w
      pure (IntermediateResult wave')
  where
    findAssignment :: Array U Ix1 Bool -> Ix1
    findAssignment arr =
      case elemIndex True (Massiv.toList arr) of
        Just i -> i
        Nothing -> panic "No valid assignment left"

-- | Given an array of non-negative values, return a random index with
-- probability proportional to the value at that index. If all values
-- are zero, the index is drawn uniformly at random.
sampleArray :: MonadRandom m => Array r Ix1 e -> m Ix1
sampleArray _ = pure 0

data ObservationResult
  = ObsContradiction
  | FinalResult (Array U Ix2 Ix1)
  | IntermediateResult (Array B Ix2 (Array U Ix1 Bool))

data EntropyResult a
  = Contradiction
  | Zero
  | Entropy a
  deriving Functor

instance Semigroup a => Monoid (EntropyResult a) where
  mempty = Zero
  mappend = (<>)

instance Semigroup a => Semigroup (EntropyResult a) where
  Contradiction <> _ = Contradiction
  _ <> Contradiction = Contradiction
  Zero <> a = a
  a <> Zero = a
  Entropy a <> Entropy b = Entropy (a <> b)

entropy :: Array U Ix1 Int -> Array U Ix1 Bool -> EntropyResult Double
entropy stationary w =
  case Massiv.foldlS
         (\(!a1, !b1, !c1) (!a2, !b2, !c2) -> (a1 + a2, b1 + b2, c1 + c2))
         (0, 0, 0)
         (Massiv.zipWith
            (\count possible ->
               if possible
                 then (1, count, fromIntegral count * log (fromIntegral count))
                 else (0, 0, 0))
            stationary
            w) of
    (_, 0, _) -> Contradiction
    (amount, sum', mainSum)
      | amount == 1 -> Zero
      | amount == Massiv.size stationary -> Entropy (log (fromIntegral (Massiv.size stationary)))
      | otherwise -> Entropy (log (fromIntegral sum') - mainSum / fromIntegral sum')

minEntropy :: forall m r. MonadRandom m => Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> m (EntropyResult Ix2)
minEntropy stationary wave = do
  (noise :: Array U Ix2 Double) <- Massiv.fromLists' Massiv.Seq <$> traverse (traverse (const getRandom)) (Massiv.toLists wave)
  let r = Massiv.ifoldlS combine Zero (Massiv.zipWith (\e n -> (n +) <$> e) (Massiv.map (entropy stationary) wave) noise)
  pure (index <$> r)
  where
    combine :: EntropyResult EntropyIndex -> Ix2 -> EntropyResult Double -> EntropyResult EntropyIndex
    combine acc i e = acc <> e'
      where e' = EntropyIndex i <$> e

data EntropyIndex = EntropyIndex
  { index :: !Ix2
  , indexEntropy :: !Double
  }

instance Semigroup EntropyIndex where
  EntropyIndex i1 e1 <> EntropyIndex i2 e2
    | e2 < e1 = EntropyIndex i2 e2
    | otherwise = EntropyIndex i1 e1
