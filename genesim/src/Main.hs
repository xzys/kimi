{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Main where

import qualified Snap.Core                as S
import qualified Snap.Http.Server         as S
import           Data.Aeson               as A
import           Data.Maybe               (fromMaybe, fromJust)
import           Control.Monad.IO.Class   (liftIO)
import           GHC.Generics             (Generic)
import qualified Data.ByteString.Char8    as B
import qualified Data.Map.Strict          as M

import           Gillepsie


data SignalSet = SignalSet {
  name ::        String,
  signals ::     SignalState
} deriving (Generic, FromJSON, ToJSON, Show)

data SimulateMessage = SimulateMessage {
  reactions ::   [Reaction],
  initMols ::    MoleculeState,
  signalSets ::  [SignalSet],
  replicates ::  Int 
} deriving (Generic, FromJSON, ToJSON, Show)



data TimeSeries = TimeSeries {
  times ::       [Double],
  molecules ::   M.Map Molecule [Integer]
} deriving (Generic, FromJSON, ToJSON, Show)

data ReturnType = ReturnType {
  signal ::      String,
  timeseries ::  [TimeSeries]
} deriving (Generic, FromJSON, ToJSON, Show)

data DataMessage = DataMessage {
  results ::     [ReturnType]
} deriving (Generic, FromJSON, ToJSON, Show)


--------------------------------------------------------------------------------
-- helpers

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: S.MonadSnap m => Int -> B.ByteString -> m b 
finishEarly code str = do
  S.modifyResponse $ S.setResponseStatus code str
  S.modifyResponse $ S.addHeader "Content-Type" "text/plain"
  S.writeBS str
  S.getResponse >>= S.finishWith

maxBodyLen = 1000000
readBodyJSON :: (S.MonadSnap m, FromJSON a) => m (Either String a)
readBodyJSON = do
  body <- A.decode `fmap` S.readRequestBody maxBodyLen
  return $ case body of
    Nothing -> Left "no JSON body found"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a

parseBodyJSON :: (S.MonadSnap m, FromJSON a) => m a
parseBodyJSON = do
  res <- readBodyJSON
  case res of
    Left e -> finishEarly 400 $ B.pack e
    Right a -> return a

--------------------------------------------------------------------------------
-- handlers

shapeCS :: [CellState] -> TimeSeries
shapeCS css = 
  let ts = map time css
      allMols = M.keys $ molecules (head css :: CellState)
      extract m = map (\(CellState _ ms) -> ms M.! m) css
      mols = M.fromList $ zip allMols $ map extract allMols
  in  TimeSeries ts mols

handleSimulate :: S.Snap ()
handleSimulate = S.method S.POST $ do
  msg@(SimulateMessage rxs initMols sss reps) <- parseBodyJSON :: S.Snap SimulateMessage
  -- res <- liftIO $ flip mapM sss $ \(SignalSet n ss) -> duplicate rxs initMols ss 3 reps
  cssss <- liftIO $ flip mapM sss $ \(SignalSet n ss) -> duplicate reactions_ initMols_ signals1_ 3 100
  let rss = zip (map name sss) ((map . map) shapeCS cssss)
  S.writeLBS $ A.encode (DataMessage $ map (uncurry ReturnType) rss)


main :: IO ()
main =
  S.httpServe S.defaultConfig $ S.route [
    ("/simulate", handleSimulate)
    ]
