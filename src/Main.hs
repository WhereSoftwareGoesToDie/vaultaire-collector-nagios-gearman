{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Byteable
import Data.Nagios.Perfdata
import System.Gearman.Worker
import System.Gearman.Connection
import Control.Monad
import Control.Monad.Reader
import Options.Applicative
import Crypto.Cipher.AES
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L 

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
    collectorAES  :: AES
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

putDebug :: Show a => a -> CollectorMonad ()
putDebug msg = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    case optVerbose of
        True -> liftIO $ putStrLn (show msg) >> return ()
        False -> return ()

collector :: CollectorMonad ()
collector = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    liftIO $ runGearman optGearmanHost optGearmanPort $ runWorker optWorkerThreads $ do
        void $ addFunc (L.pack optFunctionName) processDatum Nothing
        work
    return ()
  where
    processDatum Job{..} = do
        liftIO $ putStrLn (show jobData)
        return $ Right "done"

loadKey :: String -> IO AES
loadKey fname = S.readFile fname >>= return . initAES

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op (CollectorMonad act) = do
    let CollectorOptions{..} = op
    aes <- loadKey optKeyFile
    runReaderT act $ CollectorState op aes

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector collector
