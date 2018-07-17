{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import qualified Snap.Core                as S
import qualified Snap.Http.Server         as S
import           Data.Aeson               as A
import           Data.Maybe               (fromMaybe, fromJust)

import           GHC.Generics             (Generic)
import qualified Data.ByteString.Char8    as B
import qualified Data.Map.Strict          as M

import           Gillepsie


data SimulateMessage = SimulateMessage {
  reactions ::   [Reaction],
  signals ::     M.Map String SignalState,
  initMols ::    MoleculeState,
  replicates ::  Int 
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
-- has no type right now, but needs to be used with type in order to compile
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
handleSimulate :: S.Snap ()
handleSimulate = S.method S.POST $ do
  msg <- parseBodyJSON :: S.Snap SimulateMessage
  S.writeLBS $ A.encode $ msg 

main :: IO ()
main =
  S.httpServe S.defaultConfig $ S.route [
    ("/simulate", handleSimulate)
    ]
