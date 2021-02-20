module Polysemy.Config
  ( runGetConfig,
    runWriteConfig,
    runWriteTokens,
    saveTokens,
    runGetConfigStatic,
  )
where

import Config
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map
import Data.Time
import Logger
import Polysemy
import Polysemy.BankAccount
import Polysemy.Input
import Polysemy.OAuth
import Polysemy.Output
import Polysemy.State
import Token
import Prelude

runGetConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Input Config) r
runGetConfig fp = interpret $ \case
  Input -> do
    info $ "Loading config from " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict fp
    case ecfg of
      Left e -> error e
      Right cfg -> return cfg

runGetConfigStatic :: Config -> InterpreterFor (Input Config) r
runGetConfigStatic cfg = interpret $ \case
  Input -> return cfg

runWriteTokens :: (Members '[Logger, Embed IO, Input BankAccount] r) => FilePath -> InterpreterFor (Output TokenSet) r
runWriteTokens fp = interpret $ \case
  Output cfg -> do
    info $ "Writing tokens to " ++ fp
    ecfg <- embed $ eitherDecodeFileStrict @BankInstitutionTokens fp
    institution <- getInstitution
    case ecfg of
      Left e -> error e
      Right (BankInstitutionTokens tokensCfg) -> embed $ S.writeFile fp (encodePretty @BankInstitutionTokens $ BankInstitutionTokens $ Map.insert institution cfg tokensCfg)

runWriteConfig :: (Members '[Logger, Embed IO] r) => FilePath -> InterpreterFor (Output Config) r
runWriteConfig fp = interpret $ \case
  Output cfg -> do
    info $ "Writing config to " ++ fp
    embed $ S.writeFile fp (encodePretty cfg)

doSaveEndpoint :: (Members '[Input UTCTime, State TokenSet] r) => TokenEndpoint -> Sem r ()
doSaveEndpoint endpoint = do
  originalConfig <- get @TokenSet
  currentTime <- input @UTCTime
  put (updateTokens endpoint originalConfig currentTime)

saveTokens :: (Members '[Input UTCTime, State TokenSet, OAuthM] r) => Sem r a -> Sem r a
saveTokens = intercept @OAuthM $ \case
  GetAuthCode -> getAuthCode
  ExchangeAuthCode authCode -> do
    endpoint <- exchangeAuthCode authCode
    doSaveEndpoint endpoint
    return endpoint
  ExchangeRefreshToken tkn -> do
    endpoint <- exchangeRefreshToken tkn
    doSaveEndpoint endpoint
    return endpoint
