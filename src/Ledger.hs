module Ledger where

import Data.Maybe
import Data.Text
import Data.Time
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

newtype LedgerDay = LedgerDay Day deriving stock (Eq)

instance Show LedgerDay where
  show (LedgerDay d) = formatTime defaultTimeLocale "%Y/%m/%d" d

data LedgerTransaction = LedgerTransaction
  { date :: LedgerDay,
    description :: Text,
    comments :: [Text],
    accounts :: [(Text, Maybe Int)]
  }
  deriving stock (Eq)

testt :: LedgerTransaction
testt =
  LedgerTransaction
    { date = LedgerDay $ fromGregorian 2021 2 16,
      description = "foo",
      comments = ["MD5Sum: 8196808987945b288cf7907873c4cf33"],
      accounts = [("Expenses:Amazon", Just 123)]
    }

comment2line :: Text -> String
comment2line c = "    ; " ++ unpack c

account2line :: (Text, Maybe Int) -> String
account2line (act, Just n) = "    " ++ unpack act ++ "\t£ " ++ show (fromIntegral n / 100.0 :: Double)
account2line (act, Nothing) = "    " ++ unpack act

instance Show LedgerTransaction where
  show LedgerTransaction {..} =
    show date ++ " * " ++ unpack description ++ "\n"
      ++ Prelude.unlines (comment2line <$> comments)
      ++ Prelude.unlines (account2line <$> accounts)

type Parser = Parsec Void Text

parseMd5sum :: Parser Text
parseMd5sum = pack <$> (space *> char ';' *> space *> string "MD5Sum: " *> many alphaNumChar)

md5sums :: Parser [Text]
md5sums = catMaybes <$> some ((try (Just <$> parseMd5sum) <|> (Nothing <$ many printChar)) <* newline)

testline :: Text
testline = "    ; MD5Sum: 8f101cf8af7f8219f55b5b49ad1c6f43"

testline2 :: Text
testline2 = "2021/02/16 * Ramen dayoooo ramen dayo"

testlines :: Text
testlines =
  "2021/02/16 * Ramen dayoooo ramen dayo\n\
  \    ; MD5Sum: 8196808987945b288cf7907873c4cf33\n\
  \    ; CSV: 2021-02-16,Ramen dayoooo ramen dayo,14.0,Ashley Crooks\n\
  \    Expenses:Takeaway\n\
  \    Assets:Monzo                              £ 14.0\n\
  \\n\
  \2021/02/17 * AMZNMktplace///\n\
  \    ; MD5Sum: 1c8b885e5e8b7fa60a85a5bdbdd12afb\n\
  \    ; CSV: 2021-02-17,AMZNMktplace///,-9.99,\n\
  \    Expenses:Amazon\n\
  \    Liabilities:MasterCard                   £ -9.99\n"

readDataFile :: FilePath -> IO Text
readDataFile path = pack <$> readFile path

getMd5Sums :: FilePath -> IO [Text]
getMd5Sums fp = do
  f <- readDataFile fp
  let result = parse md5sums "md5sums" f
  case result of
    Right r -> return r
    _ -> error "nope"
