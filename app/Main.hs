module Main where

import Prelude (read)
import Protolude hiding (force, many)

import Control.Monad.Trans.Resource
import Data.Conduit (ConduitT, runConduit, (.|))
import Text.XML.Stream.Parse
import Data.XML.Types (Event)

data Overlapping = Overlapping
  { ovName :: Text
  , ovPeriodic :: Bool
  , ovPeriodicInput :: Bool
  , ovN :: Int
  , ovScreenshots :: Int
  , ovSymmetry :: Int
  , ovGround :: Int
  , ovLimit :: Maybe Int
  , ovWidth :: Int
  , ovHeight :: Int
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
           periodic
           periodicInput
           n
           screenshots
           symmetry
           ground
           limit
           width
           height)
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
  traverse_ print entries
  where filePath = "samples.xml"
