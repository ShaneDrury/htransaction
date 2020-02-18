{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

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
import Data.Ord
import Data.Semigroup ((<>))
import Data.Time
import GHC.Generics
import Lib
import Network.HTTP.Req
import Options.Generic
import Polysemy
import Polysemy.Embed
import Polysemy.Input
import Polysemy.Output
import System.Exit
import System.FilePath

data Transaction
  = Transaction
      { dated_on :: Day,
        description :: String,
        amount :: String
      }
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

data Args
  = Args
      { outfile :: FilePath,
        configFile :: FilePath,
        verbose :: Bool,
        bankAccountId :: Int
      }
  deriving (Eq, Generic, Show)

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)

log' :: (Member (Output Message) r) => String -> Sem r ()
log' msg = output $ Message msg

data Config
  = Config
      { _bankAccounts :: Map.Map Int LastImported,
        _token :: String,
        _refreshToken :: String,
        _authorizationEndpoint :: String,
        _tokenEndpoint :: String,
        _clientID :: String,
        _clientSecret :: String,
        _tokenCreatedAt :: UTCTime
      }
  deriving (Eq, Generic, Show)

$(makeLenses ''Config)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)

instance ParseRecord Args

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

-- runInputOnFile :: (Members '[Embed IO] r) => FilePath -> Sem (Input (Either String [Transaction]) ': r) a -> Sem r a
-- runInputOnFile fp = interpret $ \case
--   Input -> do
--     er <- embed $ eitherDecodeFileStrict fp
--     return $ bank_transactions <$> er

runOutputOnCsv :: (Members '[Embed IO, Output Message] r) => FilePath -> Sem (Output [Transaction] ': r) a -> Sem r a
runOutputOnCsv fp = interpret $ \case
  Output tx -> do
    log' $ "Writing to " ++ fp
    embed $ S.writeFile fp (CSV.encode tx)

-- runOutputOnStdout :: (Members '[Embed IO] r) => Sem (Output S.ByteString ': r) a -> Sem r a
-- runOutputOnStdout = interpret $ \case
--   Output csv -> embed $ S.putStrLn csv

-- runOutputLastImportedOnStdout :: (Members '[Embed IO] r) => Sem (Output LastImported ': r) a -> Sem r a
-- runOutputLastImportedOnStdout = interpret $ \case
--   Output day -> embed $ print day

updateConfig :: Int -> LastImported -> Config -> Config
updateConfig bankAccount day = over bankAccounts (Map.insert bankAccount day)

runOutputLastImportedOnFile :: (Members '[Embed IO, Output Message] r) => FilePath -> Config -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp originalConfig bankAccountId = interpret $ \case
  Output day -> do
    log' $ "Writing last imported day of " ++ show day ++ " to " ++ fp
    embed $ S.writeFile fp (encode (updateConfig bankAccountId day originalConfig))

-- runInputOnStdin :: (Members '[Embed IO] r) => Sem (Input (Either String [Transaction]) ': r) a -> Sem r a
-- runInputOnStdin = interpret $ \case
--   Input -> do
--     json <- embed BS.getContents
--     return $ bank_transactions <$> eitherDecodeStrict json

runInputOnNetwork :: (Members '[Embed IO, Output Message] r) => Int -> LastImported -> BS.ByteString -> Sem (Input [Transaction] ': r) a -> Sem r a
runInputOnNetwork bankAccountId (LastImported fromDate) token = interpret $ \case
  Input -> do
    log' $ "Getting transactions from " ++ show bankAccountId ++ " after " ++ show fromDate
    embed $ bank_transactions <$> getTransactions bankAccountId fromDate token

getTransactions :: Int -> Day -> BS.ByteString -> IO TransactionsEndpoint
getTransactions bankAccountId day token = runReq defaultHttpConfig $ do
  r <-
    req
      GET
      (https "api.freeagent.com" /: "v2" /: "bank_transactions")
      NoReqBody
      jsonResponse
      ( "bank_account" =: bankAccountId
          <> "from_date" =: day
          <> "sort" =: ("dated_on" :: Text)
          <> "per_page" =: (100 :: Int)
          <> oAuth2Bearer token
          <> header "User-Agent" ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36")
      )
  return (responseBody r :: TransactionsEndpoint)

runOutputOnLog :: (Members '[Embed IO] r) => Bool -> Sem (Output Message ': r) a -> Sem r a
runOutputOnLog verbose = interpret $ \case
  Output (Message msg) -> embed $ when verbose (putStrLn msg)

-- TODO: Maybe let effects use reader for config/args

runapp Args {..} config@Config {..} day =
  runM
    . runOutputOnLog verbose
    . runOutputLastImportedOnFile configFile config bankAccountId
    . runOutputOnCsv outfile
    . runInputOnNetwork bankAccountId day (BS.pack _token)

latestTransaction :: [Transaction] -> LastImported
latestTransaction tx = LastImported $ dated_on $ maximumBy (comparing dated_on) tx

app :: (Members '[Input [Transaction], Output [Transaction], Output LastImported, Output Message] r) => Sem r ()
app = do
  tx <- input
  log' $ "Number of transactions: " ++ show (length tx)
  when (length tx == 100) (log' $ "WARNING: Number of transactions close to limit")
  unless (null tx) (output (latestTransaction tx))
  output tx

getConfig :: FilePath -> IO (Either String Config)
getConfig fp = eitherDecodeFileStrict fp

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Int,
        refresh_token :: String
      }
  deriving (Eq, Generic, Show, FromJSON)

getAccessToken clientID clientSecret authorizationCode = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: clientID
            <> "client_secret" =: clientSecret
            <> "code" =: authorizationCode
            <> "redirect_uri" =: ("https://developers.google.com/oauthplayground" :: Text)
            <> "scope" =: ("" :: Text)
            <> "grant_type" =: ("authorization_code" :: Text)
      )
      jsonResponse
      ( header "User-Agent" ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36")
      )
  return (responseBody r :: TokenEndpoint)

--   = Config
      -- { _bankAccounts :: Map.Map Int LastImported,
      --   _token :: String,
      --   _authorizationEndpoint :: String,
      --   _tokenEndpoint :: String,
      --   _clientID :: String,
      --   _clientSecret :: String,
      --   _tokenCreatedAt :: UTCTime
      -- }



updateTokenDetails :: FilePath -> TokenEndpoint -> Config -> Config
updateTokenDetails fp TokenEndpoint{..} config = config{_token=access_token, _refreshToken=refresh_token}

main :: IO ()
main = do
  options <- getRecord "Test program"
  config <- getConfig (configFile options)
  case config of
    Left e -> die e
    Right cfg ->
      case Map.lookup (bankAccountId options) (cfg ^. bankAccounts) of
        Just day -> do
          putStrLn $ "Open and copy code: " <> authorizationUrl (cfg ^. clientID)
          authorizationCode <- getLine
          at <- getAccessToken (cfg ^. clientID) (cfg ^. clientSecret) authorizationCode
          print at
          runapp options cfg day app
        Nothing -> die $ "No bankAccountId in config: " ++ (show $ bankAccountId options)

-- TokenEndpoint {access_token = "1ydJ_8vtUB1-gGShhrLZ1nOMIJb5s9i_v3uMvF8Ib", token_type = "bearer", expires_in = 604800, refresh_token = "1QYG4gVfcOSiyRgct-UvmrwAbNIpnQ--wfWiOmOgM"}

-- if access token doesn't exist
-- go through setup flow
-- save token details
-- continue

-- if access token exists
-- and still valid
-- continue

-- if access token exists
-- but invalid
-- refresh token
-- save token details if needed
-- continue
