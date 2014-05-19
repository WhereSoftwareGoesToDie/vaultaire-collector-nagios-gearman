{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Gearman.Worker
import System.Gearman.Connection
import Control.Monad

main :: IO ()
main = do
    let processDatum _ = return $ Right "test"
    runGearman "localhost" "4370" $ runWorker 2 $ do
        void $ addFunc "service" processDatum Nothing
        work
    putStrLn "done"
