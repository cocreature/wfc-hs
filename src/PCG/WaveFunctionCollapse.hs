module PCG.WaveFunctionCollapse
    ( observe
    , ObservationResult(..)
    , Model(..)
    , ModelResult(..)
    , run
    ) where

import           Protolude hiding ((<>), rotate)

import           Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import           Control.Monad.Writer (runWriter, tell)
import           Data.List (elemIndex, findIndex)
import qualified Data.Map.Strict as Map
import           Data.Massiv.Array (Array, B, Ix1, Ix2(..), M, Source, U)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv
import qualified Data.Massiv.Array.Unsafe as Massiv
import           Data.Semigroup
import qualified Data.Set as Set
import           System.Random (Random)

data Model m = Model
  { modelObserve :: Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> m ObservationResult
  , modelPropagate :: Ix2 -> Array B Ix2 (Array U Ix1 Bool) -> Array B Ix2 (Array U Ix1 Bool)
  }

data ModelResult
  = ModelResult !(Array U Ix2 Ix1)
  | ModelContradiction

run :: MonadRandom m => Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> Model m -> m ModelResult
run stationary initialWave model = go initialWave
  where
    go wave = do
      obsRes <- modelObserve model stationary wave
      case obsRes of
        ObsContradiction -> pure ModelContradiction
        FinalResult r -> pure (ModelResult r)
        Step i wave' -> go (modelPropagate model i wave')

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
      pure (Step i wave')
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
  | FinalResult !(Array U Ix2 Ix1) -- ^ For each index there is only a single assignment left which is returned
  | Step !Ix2 !(Array B Ix2 (Array U Ix1 Bool))

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
propagate :: Ix1 -> (Ix2 -> Maybe Ix2) -> (Ix1 -> Ix1 -> Ix2 -> Bool) -> Ix2 -> Array B Ix2 (Array U Ix1 Bool) -> Array B Ix2 (Array U Ix1 Bool)
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
                        then let possible' = any (\t' -> agree t t' (q - p)) w1Possibilities
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

data Periodic = Periodic | NonPeriodic

reflect :: Massiv.Unbox a => Array U Ix2 a -> Array U Ix2 a
reflect xs = Massiv.makeArray Massiv.Seq s (\(x :. y) -> Massiv.index' xs (sx - 1 - x :. y))
  where s@(sx :. _) = Massiv.size xs

rotate :: Massiv.Unbox a => Array U Ix2 a -> Array U Ix2 a
rotate xs = Massiv.makeArray Massiv.Seq (sy :. sx) (\(x :. y) -> Massiv.index' xs (sy - 1 - y :. x))
  where sx :. sy = Massiv.size xs

overlappingModel :: forall a m. (Ord (Array U Ix2 a), Eq (Array M Ix2 a), Massiv.Unbox a, MonadRandom m) => Int -> Periodic -> Int -> Array U Ix2 a -> Model m
overlappingModel n periodic symmetry image =
  Model observe undefined
  where smx :. smy = Massiv.size image
        readPattern p = Massiv.makeArray Massiv.Seq (n :. n) (\i -> read (p + i))
        patternCounts = Map.fromListWith (+) (map (\p -> (p, 1)) patterns)
        patternArray :: Array B Ix1 (Array U Ix2 a)
        patternArray = Massiv.fromList Massiv.Seq (Map.keys patternCounts)
        stationary :: Array U Ix1 Int
        stationary = Massiv.fromList Massiv.Seq (Map.elems patternCounts)
        agree :: Ix1 -> Ix1 -> Ix2 -> Bool
        agree pid0 pid1 (dx :. dy) =
          Massiv.extract' (xmin :. ymin) s p0 == Massiv.extract' ((xmin :. ymin) - (dx :. dy)) s p1
          where p0 = Massiv.index' patternArray pid0
                p1 = Massiv.index' patternArray pid1
                s = xmax - xmin :. ymax - ymin
                xmin = max 0 dx
                xmax | dx < 0 = dx + n
                     | otherwise = n
                ymin = max 0 dy
                ymax | dy < 0 = dy + n
                     | otherwise = n
        patterns =
          concatMap (take symmetry . patternVariations . readPattern) coordinates
        patternVariations p0 = [p0, p1, p2, p3, p4, p5, p6, p7]
          where p1 = reflect p0
                p2 = rotate p0
                p3 = reflect p2
                p4 = rotate p2
                p5 = reflect p4
                p6 = rotate p4
                p7 = rotate p6
        coordinates =
          case periodic of
            Periodic -> [x :. y | x <- [0 .. smx - 1], y <- [0 .. smy - 1]]
            NonPeriodic -> [x :. y | x <- [0 .. smx - n], y <- [0 .. smy - n]]
        read (x :. y) =
          case periodic of
            Periodic -> Massiv.index' image (x `mod` smx :. y `mod` smy)
            NonPeriodic -> Massiv.index' image (x :. y)

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
