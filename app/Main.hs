module Main where

import           Prelude (read)
import           Protolude hiding (force, many)

import           Control.Monad.Trans.Resource
import           Data.Conduit (ConduitT, runConduit, (.|))
import           Data.Massiv.Array (Ix2(..), S, U)
import           Data.Massiv.Array.IO (Image)
import qualified Data.Massiv.Array.IO as Massiv
import           Data.XML.Types (Event)
import           Graphics.ColorSpace (RGB)
import           Text.XML.Stream.Parse

import qualified PCG.WaveFunctionCollapse as WFC

data Overlapping = Overlapping
  { ovName :: Text
  , ovPeriodic :: WFC.Periodic
  , ovPeriodicInput :: Bool
  , ovN :: Int
  , ovScreenshots :: Int
  , ovSymmetry :: Int
  , ovGround :: Int
  , ovLimit :: Maybe Int
  , ovOutputDim :: Ix2
  } deriving (Eq, Ord, Show)

data SimpleTiled = SimpleTiled
  { stName :: Text
  , stPeriodic :: Bool
  , stScreenshots :: Int
  , stSubset :: Maybe Text
  , stLimit :: Maybe Int
  , stTextOutput :: Bool
  , stWidth :: Int
  , stHeight :: Int
  } deriving (Eq, Ord, Show)

data Entry
  = EntryOverlapping Overlapping
  | EntrySimpleTiled SimpleTiled
  deriving (Eq, Ord, Show)

parseEntry :: MonadThrow m => ConduitT Event o m (Maybe Entry)
parseEntry =
  tag
    (anyOf ["overlapping", "simpletiled"])
    (\s ->
       if s == "overlapping"
         then parseOverlapping
         else parseSimpleTiled)
    pure
  where
    parseOverlapping = do
      name <- requireAttr "name"
      periodic <- maybe False (read . toS) <$> attr "periodic"
      periodicInput <- maybe True (read . toS) <$> attr "periodicInput"
      n <- maybe 2 (read . toS) <$> attr "N"
      screenshots <- maybe 2 (read . toS) <$> attr "screenshots"
      width <- maybe 48 (read . toS) <$> attr "width"
      height <- maybe 48 (read . toS) <$> attr "height"
      symmetry <- maybe 8 (read . toS) <$> attr "symmetry"
      ground <- maybe 0 (read . toS) <$> attr "ground"
      limit <- fmap (read . toS) <$> attr "limit"
      (pure . EntryOverlapping)
        (Overlapping
           name
           (if periodic then WFC.Periodic else WFC.NonPeriodic)
           periodicInput
           n
           screenshots
           symmetry
           ground
           limit
           (width :. height))
    parseSimpleTiled = do
      name <- requireAttr "name"
      periodic <- maybe False (read . toS) <$> attr "periodic"
      screenshots <- maybe 2 (read . toS) <$> attr "screenshots"
      subset <- attr "subset"
      limit <- fmap (read . toS) <$> attr "limit"
      textOutput <- maybe False (read . toS) <$> attr "textOutput"
      width <- maybe 10 (read . toS) <$> attr "width"
      height <- maybe 10 (read . toS) <$> attr "height"
      (pure . EntrySimpleTiled)
        (SimpleTiled name periodic screenshots subset limit textOutput width height)

parseEntries :: MonadThrow m => ConduitT Event o m (Maybe [Entry])
parseEntries = tagNoAttr "samples" (many parseEntry)

main :: IO ()
main = do
  entries <- runResourceT . runConduit $ parseFile def filePath .| force "entries required" parseEntries
  for_ entries $ \e ->
    case e of
      EntryOverlapping ov -> do
        putStrLn ("Processing sample: " <> ovName ov)
        img <- Massiv.readImageAuto ("samples/" <> toS (ovName ov) <> ".png")
        let model = WFC.overlappingModel (ovN ov) (ovPeriodic ov) (ovSymmetry ov) (img :: Image U RGB Word8)
        res <- WFC.run model (ovOutputDim ov)
        case res of
          WFC.ModelContradiction -> putStrLn ("Contradiction!" :: Text)
          WFC.ModelResult img' -> Massiv.writeImage ("output/" <> toS (ovName ov) <> ".png") img'
      EntrySimpleTiled st -> putStrLn ("Ignoring simple tiled sample: " <> stName st)
  where filePath = "samples.xml"
