{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Main where

import qualified Snap.Core                as S
import qualified Snap.Http.Server         as S
import qualified Snap.Util.CORS           as S
import           Data.Aeson               as A
import           Data.Maybe               (fromMaybe, fromJust)
import           GHC.Generics             (Generic)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy.Char8     as LB
import qualified Data.Map.Strict          as M
import           Debug.Trace              (trace)

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

data ErrorMessage = ErrorMessage {
  errors ::      [String]
} deriving (Generic, FromJSON, ToJSON, Show)


--------------------------------------------------------------------------------
-- helpers

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: S.MonadSnap m => Int -> B.ByteString -> m b 
finishEarly code str = do
  S.modifyResponse $ S.setResponseStatus code str
  -- S.modifyResponse $ S.addHeader "Content-Type" "text/plain"
  S.writeBS str
  S.getResponse >>= S.finishWith

maxBodyLen = 1000000
readBodyJSON :: (S.MonadSnap m, FromJSON a) => m (Either ErrorMessage a)
readBodyJSON = do
  body <- A.decode `fmap` S.readRequestBody maxBodyLen
  return $ case body of
    Nothing -> Left $ ErrorMessage ["no JSON body found"]
    Just v -> case A.fromJSON v of
                A.Error e -> Left $ ErrorMessage [e]
                A.Success a -> Right a

parseBodyJSON :: (S.MonadSnap m, FromJSON a) => m a
parseBodyJSON = do
  res <- readBodyJSON
  case res of
    Left e -> finishEarly 400 $ (LB.toStrict . A.encode) e
    Right a -> return a

config :: (S.MonadSnap m) => S.Config m a
config = S.setHostname "0.0.0.0" $
         S.setAccessLog (S.ConfigIoLog (putStrLn . B.unpack)) $
         S.setErrorLog (S.ConfigIoLog (putStrLn . B.unpack)) $
         S.defaultConfig

--------------------------------------------------------------------------------
-- handlers

shapeCS :: [Molecule] -> [CellState] -> TimeSeries
shapeCS mols css = 
  let ts = map time css
      extract m = map (\(CellState _ ms) -> ms M.! m) css
      molcounts = M.fromList $ zip mols $ map extract mols
  in  TimeSeries ts molcounts


sss_ = [SignalSet "Signals A" signals1_, SignalSet "Signals B" signals2_]

handleSimulate :: S.Snap ()
handleSimulate = S.method S.POST $ S.applyCORS S.defaultOptions $ do
  msg@(SimulateMessage rxs initMols sss reps) <- parseBodyJSON :: S.Snap SimulateMessage
  cssss <- liftIO $ flip mapM sss_ $ \(SignalSet n ss) -> duplicate reactions_ initMols_ ss 3 100
  let tss = (map . map) (shapeCS $ allMols reactions_) cssss
  let resp = DataMessage $ map (uncurry ReturnType) $ zip (map name sss) tss
  S.writeLBS $ A.encode resp

main :: IO ()
main =
  S.httpServe config $ S.route [
    ("/simulate", handleSimulate)
    ]

{-
-- this shows errors that snap would have otherwise eaten up
main :: IO ()
main = do
  let 
  cssss <- flip mapM sss $ \(SignalSet n ss) -> trace ("signals: " ++ n) $ duplicate reactions_ initMols_ ss 3 1000
  let rss = zip (map name sss) ((map . map) shapeCS cssss)
  let resp = DataMessage $ map (uncurry ReturnType) rss
  putStrLn $ LB.unpack $ A.encode resp
-}
