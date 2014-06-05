{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Marquise.Client
import Data.Byteable
import Data.Word
import Data.Nagios.Perfdata
import System.Gearman.Worker
import System.Gearman.Connection
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Options.Applicative
import Crypto.Cipher.AES
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString.Base64 as B64

data CollectorOptions = CollectorOptions {
    optGearmanHost   :: String,
    optGearmanPort   :: String,
    optWorkerThreads :: Int,
    optVerbose       :: Bool,
    optFunctionName  :: String,
    optKeyFile       :: String
}

opts :: Parser CollectorOptions
opts = CollectorOptions
       <$> strOption
           (long "gearman-host"
            & short 'g'
            & value "localhost"
            & metavar "GEARMANHOST"
            & help "Hostname of Gearman server.")
       <*> strOption
           (long "gearman-port"
            & short 'p'
            & value "4730"
            & metavar "GEARMANPORT"
            & help "Port number Gearman server is listening on.")
       <*> option
           (long "workers" 
            & short 'w'
            & metavar "WORKERS"
            & value 2
            & help "Number of worker threads to run.")
       <*> switch
           (long "verbose"
            & short 'v'
            & help "Write debugging output to stdout.")
       <*> strOption
           (long "function-name"
            & short 'f'
            & value "check_results"
            & metavar "FUNCTION-NAME"
            & help "Name of function to register with Gearman server.")
       <*> strOption
           (long "key-file"
            & short 'k'
            & value ""
            & metavar "KEY-FILE"
            & help "File from which to read AES key to decrypt check results. If unspecified, results are assumed to be in cleartext.")

collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser = 
    info (helper <*> opts)
    (fullDesc &
        progDesc "Vaultaire collector for Nagios with mod_gearman" &
        header "vaultaire-collector-nagios-gearman - daemon to write Nagios perfdata to Vaultaire")

data CollectorState = CollectorState {
    collectorOpts :: CollectorOptions,
    collectorAES  :: Maybe AES
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

putDebug :: Show a => a -> CollectorMonad ()
putDebug msg = ask >>= liftIO . flip maybePut msg .  optVerbose . collectorOpts 

maybePut :: Show a => Bool -> a -> IO ()
maybePut orly msg = case orly of
    True -> putStrLn (show msg) >> return ()
    False -> return ()

collector :: CollectorMonad ()
collector = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    liftIO $ runGearman optGearmanHost optGearmanPort $ runWorker optWorkerThreads $ do
        void $ addFunc (L.pack optFunctionName) (processDatum optVerbose collectorAES) Nothing
        work
    return ()

getMetricId :: Perfdata -> String -> S.ByteString
getMetricId datum metric = 
    let host = (perfdataHostname datum) in
    let service = (perfdataServiceDescription datum) in
    "host:" ++ host ++ ",metric:" ++ metric ++ ",service:" service

getMetricAddresses :: Perfdata -> [(S.ByteString,Metric)]
getMetricAddresses datum = 
    zip (map (getMetricId datum) (perfdataMetrics datum)) (perfdataMetrics datum)

processDatum :: Bool -> 
                Maybe AES -> 
                Job -> 
                IO (Either JobError L.ByteString)
processDatum dbg key Job{..} = case (clearBytes key jobData) of
    Left e -> do
        maybePut dbg ("error decoding: " ++ e)
        maybePut dbg jobData
        return $ Left (Just $ L.pack e)
    Right checkResult -> do
        ((maybePut dbg) . trimNulls) checkResult
        case (perfdataFromCheckResult checkResult) of
            Left err -> do 
                putStrLn ("Error parsing check result: " ++ err) 
                return $ Left $ Just (L.pack err)
            Right datum -> do
                putStrLn ("Got datum: " ++ (show datum)) 
                return $ Right "done"
  where
    clearBytes k d = decodeJob k $ (S.concat . L.toChunks) d
    trimNulls :: S.ByteString -> S.ByteString
    trimNulls = S.reverse . (S.dropWhile ((==) (0))) . S.reverse

loadKey :: String -> IO (Either IOException AES)
loadKey fname = try $ S.readFile fname >>= return . initAES . trim 
  where
    trim = trim' . trim'
    trim' = S.reverse . S.dropWhile isBlank
    isBlank = flip elem (S.unpack " \n")

decodeJob :: Maybe AES -> S.ByteString -> Either String S.ByteString
decodeJob k d = case (B64.decode d) of 
    Right d' -> Right $ maybeDecrypt k d'
    Left e   -> Left e 
maybeDecrypt :: Maybe AES -> S.ByteString -> S.ByteString
maybeDecrypt aes ciphertext = case aes of 
    Nothing -> ciphertext -- Nothing to do, we assume the input is already in cleartext.
    Just k -> decryptECB k ciphertext

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op (CollectorMonad act) = do
    let CollectorOptions{..} = op
    case optKeyFile of
        "" -> runReaderT act $ CollectorState op Nothing
        keyFile -> do
            aes <- loadKey keyFile
            case aes of
                Left e -> do
                    putStrLn ("Error loading key: " ++ (show e)) 
                    runReaderT act $ CollectorState op Nothing
                Right k -> runReaderT act $ CollectorState op (Just k)

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector collector
