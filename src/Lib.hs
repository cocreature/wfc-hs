module Lib
    ( observe
    , ObservationResult(..)
    ) where

import           Protolude hiding ((<>))

import           Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import           Control.Monad.Writer (runWriter, tell)
import           Data.List (elemIndex, findIndex)
import           Data.Massiv.Array (Array, B, Ix1, Ix2(..), Source, U)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv
import qualified Data.Massiv.Array.Unsafe as Massiv
import           Data.Semigroup
import qualified Data.Set as Set
import           System.Random (Random)

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
sampleArray :: (MonadRandom m, Num e, Ord e, Random e, Source r Ix1 e) => Array r Ix1 e -> m Ix1
sampleArray arr =
  if total == 0
    then getRandomR (0, Massiv.size arr - 1)
    else do
      i <- getRandomR (1, total)
      case findIndex (>= i) arr' of
        Nothing -> panic "sampleArray: Total has not been reached"
        Just j -> pure j
  where total = Massiv.sum arr
        arr' = scanl' (+) 0 (Massiv.toList arr)

data ObservationResult
  = ObsContradiction -- ^ A contradiction has been found, i.e., for at least one index there is no possible assignment.
  | FinalResult (Array U Ix2 Ix1) -- ^ For each index there is only a single assignment left which is returned
  | IntermediateResult (Array B Ix2 (Array U Ix1 Bool))

data EntropyResult a
  = Contradiction -- ^ There are no possible assignments left
  | Zero -- ^ There is only a single assignment left, i.e., the entropy is 0
  | Entropy a -- ^ The result of calculating the entropy if it is non-zero.
  deriving Functor

-- | `Zero` is the identity. Take a look at the `Semigroup` instance for a description of `(<>)`.
instance Semigroup a => Monoid (EntropyResult a) where
  mempty = Zero
  mappend = (<>)

-- | `Contradiction` behaves like a `0` canceling all other
-- values. `Zero` is the identity for `(<>)`.
instance Semigroup a => Semigroup (EntropyResult a) where
  Contradiction <> _ = Contradiction
  _ <> Contradiction = Contradiction
  Zero <> a = a
  a <> Zero = a
  Entropy a <> Entropy b = Entropy (a <> b)

-- | Add two tuples.
add3Tuple :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
add3Tuple (!a1, !b1, !c1) (!a2, !b2, !c2) = (a1 + a2, b1 + b2, c1 + c2)

-- | Compute the entropy of an array of possible patterns.
entropy :: Array U Ix1 Int -> Array U Ix1 Bool -> EntropyResult Double
entropy stationary w =
  case Massiv.foldlS
         add3Tuple
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
      | amount == Massiv.size stationary ->
        -- This case is only used to avoid potential rounding errors
        Entropy (log (fromIntegral (Massiv.size stationary)))
      | otherwise ->
        Entropy (log (fromIntegral sum') - mainSum / fromIntegral sum')

-- | Return the index with the minimal entropy.
minEntropy :: MonadRandom m => Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> m (EntropyResult Ix2)
minEntropy stationary wave = do
  (noise :: Array U Ix2 Double) <- Massiv.fromLists' Massiv.Seq <$> traverse (traverse (const getRandom)) (Massiv.toLists wave)
  let r = Massiv.ifoldlS combine Zero (Massiv.zipWith (\e n -> (n +) <$> e) (Massiv.map (entropy stationary) wave) noise)
  pure (index <$> r)
  where
    combine :: EntropyResult EntropyIndex -> Ix2 -> EntropyResult Double -> EntropyResult EntropyIndex
    combine acc i e = acc <> e'
      where e' = EntropyIndex i <$> e

-- | This type only exists to provide a `Semigroup` instance.
data EntropyIndex = EntropyIndex
  { index :: !Ix2
  , indexEntropy :: !Double
  }

-- | Choose the value with the minimum entropy.
instance Semigroup EntropyIndex where
  EntropyIndex i1 e1 <> EntropyIndex i2 e2
    | e2 < e1 = EntropyIndex i2 e2
    | otherwise = EntropyIndex i1 e1

-- | `inBounds` can discard elements or transfer them, e.g., wrap around
propagate :: Ix1 -> (Ix2 -> Maybe Ix2) -> (Ix1 -> Ix1 -> Bool) -> Ix2 -> Array B Ix2 (Array U Ix1 Bool) -> Array B Ix2 (Array U Ix1 Bool)
propagate n inBounds agree start wave = runST $ do
  wave' <- Massiv.thaw wave
  let go [] visited = pure ()
      go (p : ps) visited
        | p `Set.member` visited = go ps visited
        | otherwise = do
            w1 <- Massiv.read' wave' p
            let w1Possibilities = (map fst . filter snd . zip [0..]) (Massiv.toList w1)
            newEntries <- for neighbors $ \q -> do
              w2 <- Massiv.toList <$> Massiv.read' wave' p
              let -- There is a lot of potential for optimizations here
                  (w2', Any unmodified) = runWriter $ zipWithM
                    (\t possible ->
                      if possible
                        then let possible' = any (agree t) w1Possibilities
                             in tell (Any possible') >> pure possible'
                        else pure False)
                    [0..] w2
              Massiv.write' wave' p (Massiv.fromList Massiv.Seq w2')
              pure (if unmodified then Nothing else Just q)
            go (catMaybes newEntries ++ ps) (Set.insert p visited)
        where neighbors = mapMaybe inBounds (map (p+) offsets )
              offsets = [x :. y | x <- [-(n-1)..n-1], y <- [-(n-1)..n-1]]
  go [start] Set.empty
  Massiv.unsafeFreeze Massiv.Seq wave'


-- Overlapping
-- 1. Build an index of colors
-- 2. colors[sample[x][y]] is the original color at pixel (x,y)
-- 3. iterate over all pixels (leave out the borders if not periodic)
-- 4. `ps` is an array of length 8 holding all patterns of size NxN at the current position
-- 5. `symmetry` limits the number of those patterns that we look at
-- 7. `weights` stores the count for each pattern
-- 8. `ordering` is a unique list of all patterns
-- 9. T is the number of unique patterns
-- 10. `patterns` is an array of size `T` with all patterns
-- 11. `stationary` stores the count for all patterns
-- 12. `propagator` is an array of size 2N-1×2N-1×T which stores a list of pattern that agree with a given pattern

-- Overlapping/Propagate
-- iterate the following until there is no element left:
-- iterate over dx∈[-(N-1),N-1], dy∈[-(N-1),N-1] (if in bound, else ignore or wrap around)
-- read propagator[N-1-dx][N-1-dy]
-- iterate over possible patterns of neighbor and discard the ones that no longer agree
-- if a pattern was discare
