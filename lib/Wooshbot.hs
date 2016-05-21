module Wooshbot where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Version (showVersion)
import           Data.Yaml
import           Development.GitRev
import           GHC.Generics
import           Network.IRC.Client
import qualified Paths_wooshbot as P

-- |Configuration for wooshbot
data WooshBot = WooshBot { wbHost :: Text
                         , wbPort :: Int
                         , wbTls :: Bool
                         , wbLogFile :: Maybe FilePath
                         , wbAutojoin :: [Text]
                         , wbNick :: Text
                         , wbPass :: Maybe Text
                         , wbFreq :: Double
                         }
  deriving (Eq, Show, Generic)

instance ToJSON WooshBot
instance FromJSON WooshBot

makeLensesWith abbreviatedFields ''WooshBot

-- |Parse file into a 'WooshBot'
--
-- Will throw error on parse failure
readWooshBotFile :: FilePath -> IO WooshBot
readWooshBotFile fp =
  do res <- decodeFileEither fp
     either (fail . show) return res

-- |Parse arbitrary 'ByteString' into a 'WooshBot'
--
-- Will throw error on parse failure
readWooshBot :: ByteString -> IO WooshBot
readWooshBot = either (fail . show) return . decodeEither

-- |Connect to the configuration
connect'' :: WooshBot -> IO ()
connect'' wb =
  do c <- connCfg
     start c instanceCfg'
  where instanceCfg = defaultIRCConf (wb ^. nick)
        instanceCfg' =
          instanceCfg { _channels = wb ^. autojoin
                      , _ctcpVer = versionText
                      , _eventHandlers = mappend (_eventHandlers instanceCfg)
                                                 []
                      }
        connCmd | wb ^. tls = connectWithTLS'
                | otherwise = connect'
        logger = case wb ^. logFile of
                   Nothing -> stdoutLogger
                   Just fp -> fileLogger fp
        connCfg = connCmd logger
                          (T.encodeUtf8 (wb ^. host))
                          (wb ^. port)
                          1
-- |Authorize if requested
authHandler :: WooshBot -> EventHandler ()
authHandler wb =
  EventHandler "Haskell evaluation in a private message" ENotice ef
  where ef e = case _message e of
                 Notice tgt (Right msg)
                   | (tgt == (wb ^. nick)) &&
                     (msg `startsWith` "This nickname is registered.") ->
                       case _source e of
                         User "NickServ" -> send (Privmsg "NickServ"
                                                          (Right responseCmd))
                         _ -> return ()
                   | otherwise -> return ()
                 _ -> return ()
        responseCmd = mappend "identify " (maybe mempty id (wb ^. pass))
        x `startsWith` y = T.take (T.length y) x == y

-- |Response to @CTCP VERSION@
versionText :: Text
versionText = mconcat [ "wooshbot <"
                      , sourceText
                      , ">, version "
                      , T.pack (showVersion P.version)
                      , ", git revision "
                      , T.pack (take 7 $(gitHash))
                      , " ("
                      , T.pack $(gitBranch)
                      , ")."
                      ]

-- |Response to @CTCP SOURCE@
sourceText :: Text
sourceText = "https://github.com/pharpend/wooshbot"

-- -- |Evaluate private messages sent directly to the bot
-- evalHandler :: PrivateConf -> EventHandler ()
-- evalHandler cfg =
--   EventHandler "Haskell evaluation in a private message"
--                EPrivmsg
--                ef
--   where
--     ef e =
--       -- The source has to be a user
--       case _source e of
--         User s ->
--           case _message e of
--             Privmsg tgt (Right msg)
--               -- Private message to the bot
--               | tgt == cfg ^. nick -> mapM_ send (evald s Nothing msg)
--               | otherwise -> return ()
--             _ -> return ()
--         Channel s nck ->
--           case _message e of
--             Privmsg _ (Right msg)
--               -- Message to a channel targeting the bot
--               | or $ map (startsWith msg . mappend (cfg ^. nick)) [": ", ", "] ->
--                   -- the nick plus ", " or ": "
--                   let dbl = T.length (cfg ^. nick) + 2
--                   in mapM_ send (evald s (Just nck) (T.drop dbl msg))
--               | otherwise -> return ()
--             _ -> return ()
--         _ -> return ()
