{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Gearman.Worker
import System.Gearman.Connection
import Control.Monad
import Options.Applicative

data CollectorOptions = CollectorOptions {
    optGearmanHost :: String,
    optGearmanPort :: String
}

opts :: Parser CollectorOptions
opts = CollectorOptions
       <$> strOption
           (long "gearman-host"
            <> short 'h'
            <> value "localhost"
            <> metavar "GEARMANHOST"
            <> help "Hostname of Gearman server.")
       <*> strOption
           (long "gearman-port"
            <> short 'p'
            <> value "4370"
            <> metavar "GEARMANPORT"
            <> help "Port number Gearman server is listening on.")

collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser = 
    info (helper <*> opts)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios with mod_gearman" <>
        header "vaultaire-collector-nagios-gearman - daemon to write Nagios perfdata to Vaultaire")

collector :: CollectorOptions -> IO ()
collector CollectorOptions{..} = do
    runGearman optGearmanHost optGearmanPort $ runWorker 2 $ do
        void $ addFunc "service" processDatum Nothing
        work
    putStrLn "done"
  where
    processDatum Job{..} = do
        print jobData
        return $ Right  "done"

main :: IO ()
main = execParser collectorOptionParser >>= collector
