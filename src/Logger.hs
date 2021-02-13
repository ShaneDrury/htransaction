module Logger
  ( LogMsg,
    info,
    Logger,
    runLoggerAsOutput,
    runLoggerOnRainbow,
    warn,
    err,
    LogType (..),
  )
where

import Colog.Polysemy.Effect
import qualified Data.Text as T
import Polysemy
import Polysemy.Output
import Rainbow
import Prelude hiding (log)

data LogType = Info | Warning | LogError deriving stock (Eq, Show)

type LogMsg = (LogType, String)

type Logger = Log LogMsg

info :: Members '[Logger] r => String -> Sem r ()
info s = log (Info, s)

warn :: Members '[Logger] r => String -> Sem r ()
warn s = log (Warning, s)

err :: Members '[Logger] r => String -> Sem r ()
err s = log (LogError, s)

runLoggerOnRainbow :: (Members '[Embed IO] r) => InterpreterFor (Log LogMsg) r
runLoggerOnRainbow =
  interpret
    ( \(Log (logType, s)) -> case logType of
        Info -> embed $ putChunkLn (msg s)
        Warning -> embed $ putChunkLn $ fore yellow $ bold (msg s)
        LogError -> embed $ putChunkLn $ fore red $ bold (msg s)
    )
  where
    msg = chunk . T.pack

runLoggerAsOutput :: Sem (Log LogMsg : r) a -> Sem (Output LogMsg : r) a
runLoggerAsOutput =
  reinterpret
    ( \(Log logmsg) -> output logmsg
    )
