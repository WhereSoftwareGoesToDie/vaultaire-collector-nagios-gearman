{-# LANGUAGE OverloadedStrings #-}

module Perfdata where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.Word
import Data.List hiding (takeWhile)
import qualified Data.Map as M

data Item = Item {
    name :: S.ByteString,
    value :: S.ByteString
} deriving (Show)

separator :: Parser [Word8]
separator = count 2 (char8 ':') <?> "separator"

ident :: Parser S.ByteString
ident = takeWhile uppercase
  where
    uppercase = inClass $ enumFromTo 'A' 'Z'

val :: Parser S.ByteString
val = takeTill isTabOrEol
  where
    isTabOrEol c = (c == '\t' || c == '\n')

item :: Parser Item
item = Item `fmap` ident <* separator <*> val

line :: Parser [Item]
line = many item

type ItemMap = M.Map S.ByteString S.ByteString

data MetricValue = Int64 | Float64

type MetricMap = M.Map S.ByteString MetricValue

mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (name i) (value i) m) M.empty

data ServicePerfdata = ServicePerfdata {
    serviceDescription :: S.ByteString,
    serviceState       :: S.ByteString
}

data HostOrService = Service ServicePerfdata | Host

data Perfdata = Perfdata {
    dataType :: HostOrService,
    timestamp :: Word64,
    hostname :: S.ByteString,
    hostState :: S.ByteString,
    hostStateType :: S.ByteString,
    perfMetrics   :: MetricMap
}

type ParserError = [Char]

parseLine :: S.ByteString -> Result [Item]
parseLine = parse line

extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is) = Right $ mapItems is
extractItems (Fail _ ctxs err) = Left $ concat $ err : "\n" : (intercalate "," ctxs) : []
extractItems (Partial f) = extractItems (f "")

type MaybeError = Maybe ParserError

type MaybePerfdata = Maybe Perfdata

type ErrorState = State MaybeError

parseServiceData :: ItemMap -> ErrorState (Maybe ServicePerfdata)
parseServiceData m = case (M.lookup "SERVICEDESC" m) of
    Nothing -> do
        put $ Just "SERVICEDESC not found"
        return Nothing
    Just desc -> case (M.lookup "SERVICESTATE" m) of
        Nothing -> do
            put $ Just "SERVICESTATE not found"
            return Nothing
        Just state -> return $ Just $ ServicePerfdata desc state

parseDataType :: ItemMap -> ErrorState (Maybe HostOrService)
parseDataType m = case (M.lookup "DATATYPE" m) of
    Nothing -> do
        put $ Just "DATATYPE not found"
        return Nothing
    Just s -> case s of
        "HOSTPERFDATA" -> return $ Just Host
        "SERVICEPERFDATA" -> do
            serviceData <- parseServiceData m
            case serviceData of
                Nothing -> return Nothing
                Just d -> return $ Just $ Service d

extractPerfdata :: ItemMap -> Either ParserError Perfdata
extractPerfdata m = do
    let (datum,err) =  flip runState Nothing $ do 
                           dataType <- parseDataType m
                           return Nothing
    Left ""
