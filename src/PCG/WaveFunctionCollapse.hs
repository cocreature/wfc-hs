module PCG.WaveFunctionCollapse
    ( Model(..)
    , ModelResult(..)
    , run
    , overlappingModel
    , Periodic(..)
    ) where

import           Protolude hiding ((<>), rotate)

import           Control.Monad.Random.Class (MonadRandom, getRandom, getRandomR)
import           Control.Monad.Writer (runWriter, tell)
import           Data.List (elemIndex, findIndex)
import qualified Data.Map.Strict as Map
import           Data.Massiv.Array (Array, B, Ix1, Ix3, Ix2(..), IxN(..), M, Source, U)
import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array.Mutable as Massiv
import qualified Data.Massiv.Array.Unsafe as Massiv
import           Data.Semigroup
import qualified Data.Set as Set
import           System.Random (Random)

data Model m a = Model
  { modelObserve :: Array B Ix2 (Array U Ix1 Bool) -> m ObservationResult
  , modelPropagate :: Ix2 -> Array B Ix2 (Array U Ix1 Bool) -> Array B Ix2 (Array U Ix1 Bool)
  , modelOutput :: Array U Ix2 Ix1 -> Array U Ix2 a
  , modelNumPatterns :: Ix1
  }

data ModelResult a
  = ModelResult !(Array U Ix2 a)
  | ModelContradiction

run :: MonadRandom m => Model m a -> Ix2 -> m (ModelResult a)
run model outputDim = go initialWave
  where
    initialWave =
      Massiv.makeArray
        Massiv.Seq
        outputDim
        (const (Massiv.makeArray Massiv.Seq (modelNumPatterns model) (const True)))
    go wave = do
      obsRes <- modelObserve model wave
      case obsRes of
        ObsContradiction -> pure ModelContradiction
        FinalResult r -> pure (ModelResult (modelOutput model r))
        Step i wave' -> go (modelPropagate model i wave')

observe :: MonadRandom m => Array U Ix1 Int -> Array B Ix2 (Array U Ix1 Bool) -> m ObservationResult
observe stationary wave = do
  r <- minEntropy stationary wave
  traceShowM r
  case r of
    Contradiction -> pure ObsContradiction
    Zero -> pure (FinalResult (Massiv.compute (Massiv.map findAssignment wave)))
    Entropy i -> do
      val <- sampleArray (Massiv.zipWith (\possible count -> if possible then count else 0) (wave Massiv.! i) stationary)
      traceShowM stationary
      traceM ("setting " <> show i <> " to " <> show val)
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
        arr' = drop 1 (scanl' (+) 0 (Massiv.toList arr))

data ObservationResult
  = ObsContradiction -- ^ A contradiction has been found, i.e., for at least one index there is no possible assignment.
  | FinalResult !(Array U Ix2 Ix1) -- ^ For each index there is only a single assignment left which is returned
  | Step !Ix2 !(Array B Ix2 (Array U Ix1 Bool))

data EntropyResult a
  = Contradiction -- ^ There are no possible assignments left
  | Zero -- ^ There is only a single assignment left, i.e., the entropy is 0
  | Entropy a -- ^ The result of calculating the entropy if it is non-zero.
  deriving (Show, Functor)

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
  , _indexEntropy :: !Double
  }

-- | Choose the value with the minimum entropy.
instance Semigroup EntropyIndex where
  EntropyIndex i1 e1 <> EntropyIndex i2 e2
    | e2 < e1 = EntropyIndex i2 e2
    | otherwise = EntropyIndex i1 e1

-- | `inBounds` can discard elements or transfer them, e.g., wrap around
propagate :: Ix1 -> (Ix2 -> Ix2 -> Maybe Ix2) -> (Ix1 -> Ix1 -> Ix2 -> Bool) -> Ix2 -> Array B Ix2 (Array U Ix1 Bool) -> Array B Ix2 (Array U Ix1 Bool)
propagate n inBounds agree start wave = runST $ do
  wave' <- Massiv.thaw wave
  let go [] _ = pure ()
      go (p : ps) visited
        | p `Set.member` visited = go ps visited
        | otherwise = do
            -- traceM ("Propagating from " <> show p)
            w1 <- Massiv.read' wave' p
            let w1Possibilities = (map fst . filter snd . zip [0..]) (Massiv.toList w1)
            -- traceShowM w1Possibilities
            newEntries <- for neighbors $ \(q, d) -> do
              w2 <- Massiv.toList <$> Massiv.read' wave' q
              -- traceM ("Propagating to " <> show q)
              let -- There is a lot of potential for optimizations here
                  (w2', Any modified) = runWriter $ zipWithM
                    (\t possible ->
                      if possible
                        then let possible' = any (\t' -> agree t t' d) w1Possibilities
                             in tell (Any (not possible')) >> pure possible'
                        else pure False)
                    [0..] w2
              -- traceM (show modified <> ": " <> show w2 <> " → " <> show w2')
              if modified
                then do
                  Massiv.write' wave' q (Massiv.fromList Massiv.Seq w2')
                  pure (Just q)
                else pure Nothing
            go (catMaybes newEntries ++ ps) (Set.insert p visited)
        where neighbors = mapMaybe (\d -> (\q -> (q, d)) <$> inBounds (Massiv.size wave) (p + d)) offsets
              offsets = [x :. y | x <- [-(n-1)..n-1], y <- [-(n-1)..n-1]]
  go [start] Set.empty
  Massiv.unsafeFreeze Massiv.Seq wave'

data Periodic = Periodic | NonPeriodic
  deriving (Eq, Ord, Show)

reflect :: Massiv.Unbox a => Array U Ix2 a -> Array U Ix2 a
reflect xs = Massiv.makeArray Massiv.Seq s (\(x :. y) -> Massiv.index' xs (sx - 1 - x :. y))
  where s@(sx :. _) = Massiv.size xs

rotate :: Massiv.Unbox a => Array U Ix2 a -> Array U Ix2 a
rotate xs = Massiv.makeArray Massiv.Seq (sy :. sx) (\(x :. y) -> Massiv.index' xs (sy - 1 - y :. x))
  where sx :. sy = Massiv.size xs

overlappingModel :: forall a m. (Ord a, Massiv.Unbox a, MonadRandom m) => Int -> Periodic -> Int -> Array U Ix2 a -> Model m a
overlappingModel n periodic symmetry image =
  Model
    { modelObserve = observe stationary
    , modelPropagate = propagate n inBounds agree'
    , modelOutput = output
    , modelNumPatterns = Massiv.size patternArray
    }
  where smx :. smy = Massiv.size image
        inBounds (fmx :. fmy) (x :. y) =
          case periodic of
            Periodic -> pure (x `mod` fmx :. y `mod` fmy)
            NonPeriodic -> do
              guard (x >= 0 && x <= fmx - n)
              guard (y >= 0 && y <= fmy - n)
              pure (x `mod` smx :. y `mod` smy)
        output res =
          Massiv.makeArray Massiv.Seq (Massiv.size res) $
          \(x :. y) ->
            let dx = if x < fmx - n + 1 then 0 else n - 1
                dy = if y < fmy - n + 1 then 0 else n - 1
            in Massiv.index' (Massiv.index' patternArray (Massiv.index' res (x - dx :. y - dy))) (dx :. dy)
          where fmx :. fmy = Massiv.size res
        readPattern p = Massiv.makeArray Massiv.Seq (n :. n) (\i -> read (p + i))
        patternCounts = Map.fromListWith (+) (map (\p -> (p, 1)) patterns)
        patternArray :: Array B Ix1 (Array U Ix2 a)
        patternArray = Massiv.fromList Massiv.Seq (Map.keys patternCounts)
        stationary :: Array U Ix1 Int
        stationary = Massiv.fromList Massiv.Seq (Map.elems patternCounts)
        t = Massiv.size patternArray
        propagator :: Array B Ix3 (Set Int)
        propagator =
          Massiv.makeArray Massiv.Seq (2 * n - 1 :> 2 * n - 1 :. t)
          (\(x :> y :. p) -> Set.fromList (filter (\p' -> agree p p' (x - n + 1 :. y - n + 1)) [0 .. t - 1]))
        agree' :: Ix1 -> Ix1 -> Ix2 -> Bool
        agree' pid0 pid1 (dx :. dy) =
          pid1 `Set.member` Massiv.index' propagator (n - 1 - dx :> n - 1 - dy :. pid0)
        agree :: Ix1 -> Ix1 -> Ix2 -> Bool
        agree pid0 pid1 (dx :. dy) =
          Massiv.convert (Massiv.extract' (xmin :. ymin) s p0) ==
          (Massiv.convert (Massiv.extract' ((xmin :. ymin) - (dx :. dy)) s p1) :: Array U Ix2 a)
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
