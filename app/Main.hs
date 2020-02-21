{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Cli
import Config
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Csv as CSV
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Tagged
import Data.Text hiding (drop, length, null)
import Data.Time
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Req
import Polysemy
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import System.Exit
import Token
import Transaction
import Types

log' :: (Member (Output Message) r) => String -> Sem r ()
log' msg = output $ Message msg

runInputTest :: (Members '[Embed IO] r) => Sem (Input [Transaction] ': r) a -> Sem r a
runInputTest = interpret $ \case
  Input -> return []

runOutputOnCsv :: (Members '[Embed IO, Output Message] r) => FilePath -> Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    log' $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnStdout = interpret $ \case
  Output tx -> embed $ S.putStrLn (CSV.encode tx)

runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnStdout = interpret $ \case
  Output day -> embed $ print day

runOutputLastImportedOnFile :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- input
    log' $ "Writing last imported day of " ++ show day ++ " to " ++ fp
    embed $ S.writeFile fp (encode (updateConfig bankAccountId day originalConfig))

runSaveTokens :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokens fp = interpret $ \case
  Output tokens -> do
    originalConfig <- input
    newConfig <- embed $ withNewTokens tokens originalConfig
    embed $ S.writeFile fp (encode newConfig)

runGetAccessTokens :: (Members '[Embed IO, Input Config, Output Message, Output TokenEndpoint] r) => Sem (Input (Tagged AccessToken TokenEndpoint) ': r) a -> Sem r a
runGetAccessTokens = interpret $ \case
  Input -> do
    config <- input
    embed $ putStrLn $ "Open and copy code: " <> authorizationUrl (config ^. clientID)
    authorizationCode <- embed getLine
    getAccessToken (config ^. clientID) (config ^. clientSecret) authorizationCode

runUseRefreshTokens :: (Members '[Embed IO, Input Config, Output Message, Output TokenEndpoint] r) => Sem (Input (Tagged Refresh TokenEndpoint) ': r) a -> Sem r a
runUseRefreshTokens = interpret $ \case
  Input -> do
    config <- input
    log' "Trying to refresh tokens"
    useRefreshToken (config ^. clientID) (config ^. clientSecret) (fromJust (config ^. refreshToken))

runValidToken :: (Members '[Embed IO, Input Config, Input (Tagged AccessToken TokenEndpoint), Input (Tagged Refresh TokenEndpoint), Output Message, Output TokenEndpoint] r) => Sem (Input ValidToken ': r) a -> Sem r a
runValidToken = interpret $ \case
  Input -> do
    config <- input
    case config ^. token of
      Just t -> do
        case config ^. tokenExpiresAt of
          Just expires -> do
            currentTime <- embed getCurrentTime
            if expires <= currentTime
              then refreshTokens
              else return $ ValidToken $ BS.pack t
          Nothing -> getSaveTokens
        return $ ValidToken $ BS.pack t
      Nothing -> getSaveTokens
    where
      getSaveTokens = do
        tokens <- input @(Tagged AccessToken TokenEndpoint)
        output $ unTagged tokens
        return $ ValidToken $ BS.pack $ access_token (unTagged tokens)
      refreshTokens = do
        tokens <- input @(Tagged Refresh TokenEndpoint)
        output $ unTagged tokens
        return $ ValidToken $ BS.pack $ access_token (unTagged tokens)

runInputOnNetwork :: (Members '[Embed IO, Input LastImported, Output Message, Input ValidToken] r) => Int -> Sem (Input [Transaction] ': r) a -> Sem r a
runInputOnNetwork bankAccountId = interpret $ \case
  Input -> do
    (LastImported fromDate) <- input
    token <- input @ValidToken
    log' $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
    embed $ bank_transactions <$> getTransactions bankAccountId fromDate token

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Output Message ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Output (Message msg) -> embed $ when verbose (putStrLn msg)

runGetConfig :: (Members '[Embed IO] r) => FilePath -> Sem (Input Config ': r) a -> Sem r a
runGetConfig fp = interpret $ \case
  Input -> do
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> embed $ die e
      Right cfg -> return cfg

runGetLastImported :: (Members '[Input Config] r) => Int -> Sem (Input LastImported ': r) a -> Sem r a
runGetLastImported bankAccountId = interpret $ \case
  Input -> do
    cfg <- input @Config
    case Map.lookup bankAccountId (cfg ^. bankAccounts) of
      Just day -> return day
      Nothing -> error $ "No bankAccountId in config: " ++ show bankAccountId

runGetConfigTest :: Sem (Input Config ': r) a -> Sem r a
runGetConfigTest = interpret $ \case
  Input ->
    return $
      Config
        { _bankAccounts = Map.fromList [(123, LastImported $ fromGregorian 2020 02 04), (679673, LastImported $ fromGregorian 2020 02 07)],
          _token = Just "token",
          _refreshToken = Just "refresh",
          _authorizationEndpoint = "https://api.freeagent.com/v2/approve_app",
          _tokenEndpoint = "https://api.freeagent.com/v2/token_endpoint",
          _clientID = "clientid",
          _clientSecret = "secret",
          _tokenExpiresAt = Nothing
        }

runSaveTokensStdout :: (Members '[Embed IO] r) => Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokensStdout = interpret $ \case
  Output tokens -> embed $ print tokens

app :: (Members '[Input [Transaction], Output [Transaction], Output LastImported, Output Message] r) => Sem r ()
app = do
  tx <- input
  log' $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (log' "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

runapp Args {..} =
  runM
    . runGetConfig configFile
    . runOutputOnLog verbose
    . runOutputLastImportedOnFile configFile bankAccountId
    . runOutputOnCsv outfile
    . runSaveTokens configFile
    . runUseRefreshTokens
    . runGetLastImported bankAccountId
    . runGetAccessTokens
    . runValidToken
    . runInputOnNetwork bankAccountId

main :: IO ()
main = do
  options <- getArgs
  runapp options app
