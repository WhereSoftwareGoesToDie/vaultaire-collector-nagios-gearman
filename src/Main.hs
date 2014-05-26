{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Gearman.Worker
import System.Gearman.Connection
import Control.Monad
import Control.Monad.Reader
import Options.Applicative
import qualified Data.ByteString as S

data CollectorOptions = CollectorOptions {
    optGearmanHost :: String,
    optGearmanPort :: String,
    optWorkerThreads :: Int,
    optVerbose       :: Bool
}

opts :: Parser CollectorOptions
opts = CollectorOptions
       <$> strOption
           (long "gearman-host"
            <> short 'g'
            <> value "localhost"
            <> metavar "GEARMANHOST"
            <> help "Hostname of Gearman server.")
       <*> strOption
           (long "gearman-port"
            <> short 'p'
            <> value "4730"
            <> metavar "GEARMANPORT"
            <> help "Port number Gearman server is listening on.")
       <*> option
           (long "workers" 
            <> short 'w'
            <> metavar "WORKERS"
            <> value 2
            <> showDefault
            <> help "Number of worker threads to run.")
       <*> switch
           (long "verbose"
            <> short 'v'
            <> help "Write debugging output to stdout.")

collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser = 
    info (helper <*> opts)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios with mod_gearman" <>
        header "vaultaire-collector-nagios-gearman - daemon to write Nagios perfdata to Vaultaire")

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorOptions IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorOptions)

putDebug :: Show a => a -> CollectorMonad ()
putDebug msg = do
    CollectorOptions{..} <- ask
    case optVerbose of
        True -> liftIO $ putStrLn (show msg) >> return ()
        False -> return ()

collector :: CollectorMonad ()
collector = do
    CollectorOptions{..} <- ask
    liftIO $ runGearman optGearmanHost optGearmanPort $ runWorker optWorkerThreads $ do
        void $ addFunc "check_results" processDatum Nothing
        work
    return ()
  where
    processDatum Job{..} = return $ Right "done"

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op (CollectorMonad act) = runReaderT act op

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector collector
