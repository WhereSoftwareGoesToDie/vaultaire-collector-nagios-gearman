{-# LANGUAGE OverloadedStrings #-}

module Perfdata where

import Prelude hiding (takeWhile)
import Data.Int
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.ByteString.Char8 (readInteger, pack)
import Data.Word
import Data.List hiding (takeWhile)
import qualified Data.Map as M

data Item = Item {
    label :: S.ByteString,
    content :: S.ByteString
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

data MetricValue = DoubleValue Double | UnknownValue deriving (Show)

data Threshold = DoubleThreshold Double | NoThreshold deriving (Show)

data Metric = Metric {
    metricValue :: MetricValue,
    metricUOM   :: UOM,
    warnValue :: Threshold,
    critValue :: Threshold,
    minValue :: Threshold,
    maxValue :: Threshold
} deriving (Show)

type MetricList = [([Char], Metric)]

data UOM = Second | Millisecond | Microsecond | Percent | Byte | Kilobyte | Megabyte | Terabyte | Counter | NullUnit | UnknownUOM
    deriving (Show)

uomFromString :: [Char] -> UOM
uomFromString "s" = Second 
uomFromString "ms" = Millisecond
uomFromString "us" = Microsecond
uomFromString "%" = Percent
uomFromString "b" = Byte
uomFromString "kb" = Kilobyte
uomFromString "mb" = Megabyte
uomFromString "tb" = Terabyte
uomFromString "c" = Counter
uomFromString "" = NullUnit
uomFromString _ = UnknownUOM

mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (label i) (content i) m) M.empty

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
    perfMetrics   :: MetricList
}

type ParserError = [Char]

parseLine :: S.ByteString -> Result [Item]
parseLine = parse line

fmtParseError :: [String] -> String -> String
fmtParseError ctxs err = concat $ err : "\n" : (intercalate "," ctxs) : []

extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is) = Right $ mapItems is
extractItems (Fail _ ctxs err) = Left $ fmtParseError ctxs err
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
        Just sState -> return $ Just $ ServicePerfdata desc sState

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
        _                 -> do 
                            put $ Just  "Invalid datatype"
                            return Nothing

parseHostname :: ItemMap -> ErrorState (Maybe S.ByteString)
parseHostname m = case (M.lookup "HOSTNAME" m) of
    Nothing -> do
        put $ Just "HOSTNAME not found"
        return Nothing
    Just h -> return $ Just h

parseTimestamp :: ItemMap -> ErrorState (Maybe Int64)
parseTimestamp m = case (M.lookup "TIMET" m) of
    Nothing -> do
        put $ Just "TIMET not found"
        return Nothing
    Just t  -> case (readInteger t) of
        Nothing -> do
            put $ Just "Invalid timestamp"
            return Nothing
        Just (n, _) -> return $ Just $ fromInteger n

parseHostState :: ItemMap -> ErrorState (Maybe S.ByteString)
parseHostState m = case (M.lookup "HOSTSTATE" m) of
    Nothing -> do
        put $ Just "HOSTSTATE not found"
        return Nothing
    Just s -> return $ Just s

uom :: Parser UOM
uom = option "" (many letter_ascii) >>= (return . uomFromString)

metricName :: Parser [Char]
metricName = (option quote (char quote)) *>
             (many (satisfy nameChar)) <*
             (option quote (char quote))
  where
    quote = '\''
    nameChar '\'' = False
    nameChar '='  = False
    nameChar _    = True

value :: Parser MetricValue
value = option UnknownValue (double >>= (return . DoubleValue))

threshold :: Parser Threshold
threshold = (char8 ';') *> option NoThreshold (double >>= (return . DoubleThreshold))

metric :: Parser ([Char], Metric)
metric = do
    name <- metricName
    void $ char8 '='
    m    <- Metric `fmap` value <*>
                          uom <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) 
    return (name, m)

metricLine :: Parser MetricList
metricLine = many (metric <* (skipMany (char8 ';') <* skipSpace))

parseMetricString :: S.ByteString -> Either ParserError MetricList
parseMetricString mStr = completeParse (parse metricLine mStr)
  where
    completeParse r = case r of
        Done _ m -> Right m
        Fail _ ctxs err -> Left $ fmtParseError ctxs err
        Partial parseRest -> completeParse (parseRest "")

parseHostMetrics :: ItemMap -> Either ParserError MetricList
parseHostMetrics m = case (M.lookup "HOSTPERFDATA" m) of
    Nothing -> Left "HOSTPERFDATA not found"
    Just p  -> parseMetricString p

parseServiceMetrics :: ItemMap -> Either ParserError MetricList
parseServiceMetrics m = case (M.lookup "SERVICEPERFDATA" m) of
    Nothing -> Left "SERVICEPERFDATA not found"
    Just p  -> parseMetricString p

parseMetrics :: HostOrService -> ItemMap -> ErrorState (Maybe MetricList)
parseMetrics typ m = do
    case typ of
        Host -> case (parseHostMetrics m) of
            Left err -> do
                put $ Just err
                return Nothing
            Right metrics -> return $ Just metrics
        Service _ -> case (parseServiceMetrics m) of
            Left err -> do
                put $ Just err
                return Nothing
            Right metrics -> return $ Just metrics

extractPerfdata :: ItemMap -> Either ParserError Perfdata
extractPerfdata m = do
    let (datum,err) =  flip runState Nothing $ do 
                           dType <- parseDataType m
                           hName <- parseHostname m
                           tStamp <- parseTimestamp m
                           hState <- parseHostState m
                           return Nothing
    Left ""
