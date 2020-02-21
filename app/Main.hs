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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Cli
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

data Transaction
  = Transaction
      { dated_on :: Day,
        description :: String,
        amount :: String
      }
  deriving (Eq, Generic, Show, FromJSON, CSV.ToRecord)

instance CSV.ToField Day where
  toField d = BS.pack $ showGregorian d

newtype LastImported = LastImported Day deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Message = Message String deriving (Eq, Show)

log' :: (Member (Output Message) r) => String -> Sem r ()
log' msg = output $ Message msg

data Config
  = Config
      { _bankAccounts :: Map.Map Int LastImported,
        _token :: Maybe String,
        _refreshToken :: Maybe String,
        _authorizationEndpoint :: String,
        _tokenEndpoint :: String,
        _clientID :: String,
        _clientSecret :: String,
        _tokenExpiresAt :: Maybe UTCTime
      }
  deriving (Eq, Generic, Show)

$(makeLenses ''Config)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Config)

newtype TransactionsEndpoint
  = TransactionsEndpoint
      { bank_transactions :: [Transaction]
      }
  deriving (Eq, Generic, Show, FromJSON)

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

updateConfig :: Int -> LastImported -> Config -> Config
updateConfig bankAccount day = over bankAccounts (Map.insert bankAccount day)

runOutputLastImportedOnFile :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Int -> Sem (Output LastImported ': r) a -> Sem r a
runOutputLastImportedOnFile fp bankAccountId = interpret $ \case
  Output day -> do
    originalConfig <- input
    log' $ "Writing last imported day of " ++ show day ++ " to " ++ fp
    embed $ S.writeFile fp (encode (updateConfig bankAccountId day originalConfig))

withNewTokens :: TokenEndpoint -> Config -> IO Config
withNewTokens TokenEndpoint {..} original = do
  currentTime <- getCurrentTime
  case refresh_token of
    Just rt ->
      return $
        original
          { _token = Just access_token,
            _refreshToken = refresh_token,
            _tokenExpiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
          }
    Nothing ->
      return $
        original
          { _token = Just access_token,
            _tokenExpiresAt = Just $ addUTCTime (fromIntegral expires_in) currentTime
          }

runSaveTokens :: (Members '[Embed IO, Input Config, Output Message] r) => FilePath -> Sem (Output TokenEndpoint ': r) a -> Sem r a
runSaveTokens fp = interpret $ \case
  Output tokens -> do
    originalConfig <- input
    newConfig <- embed $ withNewTokens tokens originalConfig
    embed $ S.writeFile fp (encode newConfig)

newtype ValidToken = ValidToken BS.ByteString

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

getTransactions :: Int -> Day -> ValidToken -> IO TransactionsEndpoint
getTransactions bankAccountId day (ValidToken token) = runReq defaultHttpConfig $ do
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
          <> header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  return (responseBody r :: TransactionsEndpoint)

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

latestTransaction :: [Transaction] -> LastImported
latestTransaction tx = LastImported $ dated_on $ maximumBy (comparing dated_on) tx

authorizationUrl :: String -> String
authorizationUrl clientId = "https://api.freeagent.com/v2/approve_app?client_id=" <> clientId <> "&response_type=code&redirect_uri=https%3A%2F%2Fdevelopers.google.com%2Foauthplayground"

data Refresh = Refresh

data AccessToken = AccessToken

data TokenEndpoint
  = TokenEndpoint
      { access_token :: String,
        token_type :: String,
        expires_in :: Integer,
        refresh_token :: Maybe String
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
      ( header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  let body = responseBody r :: TokenEndpoint
  return $ Tagged @AccessToken body

useRefreshToken clientID clientSecret refreshToken = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (https "api.freeagent.com" /: "v2" /: "token_endpoint")
      ( ReqBodyUrlEnc $
          "client_id" =: clientID
            <> "client_secret" =: clientSecret
            <> "refresh_token" =: refreshToken
            <> "grant_type" =: ("refresh_token" :: Text)
      )
      jsonResponse
      ( header "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
      )
  let body = responseBody r :: TokenEndpoint
  return $ Tagged @Refresh body

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
