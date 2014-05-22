{-# LANGUAGE OverloadedStrings #-}

module Perfdata(
    perfdataFromByteString,
    Perfdata,
    MetricList,
    Metric,
    HostOrService,
    ServicePerfdata,
    ParserError
) where

import Prelude hiding (takeWhile)
import Data.Int
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.ByteString.Char8 (readInteger)
import Data.Word
import Data.List hiding (takeWhile)
import qualified Data.Map as M

data Item = Item {
    label :: S.ByteString,
    content :: S.ByteString
} deriving (Show)

-- |Matches the '::' separating items in check result output.
separator :: Parser [Word8]
separator = count 2 (char8 ':') <?> "separator"

-- |Matches the key in check result output.
ident :: Parser S.ByteString
ident = takeWhile uppercase
  where
    uppercase = inClass $ enumFromTo 'A' 'Z'

-- |Matches the value in check result output.
val :: Parser S.ByteString
val = takeTill isTabOrEol
  where
    isTabOrEol c = (c == '\t' || c == '\n')

-- |Matches a key::value pair in check result output.
item :: Parser Item
item = Item `fmap` ident <* separator <*> val

-- |Matches a line of key::value pairs (i.e., the result of one check). 
line :: Parser [Item]
line = many item

-- |Map from key to value for items in a check result.
type ItemMap = M.Map S.ByteString S.ByteString

-- |Value of a performance metric. We may lose some data converting 
-- to doubles here; this may change in the future.
data MetricValue = DoubleValue Double | UnknownValue deriving (Show)

-- |Value of a min/max/warn/crit threshold, subject to the same 
-- constraints as MetricValue.
data Threshold = DoubleThreshold Double | NoThreshold deriving (Show)

-- |Encapsulates the data in a Nagios performance metric. A service can
-- have several of these.
data Metric = Metric {
    metricValue :: MetricValue,
    metricUOM   :: UOM,
    warnValue :: Threshold,
    critValue :: Threshold,
    minValue :: Threshold,
    maxValue :: Threshold
} deriving (Show)

-- |List of metrics by metric name.
type MetricList = [([Char], Metric)]

-- |Nagios unit of measurement. NullUnit is an empty string in the 
-- check result; UnknownUOM indicates a failure to parse.
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

-- |Insert items from a list into a map for easy access by key.
mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (label i) (content i) m) M.empty

-- |The part of the check result that's specific to service checks, 
-- and doesn't appear in host checks.
data ServicePerfdata = ServicePerfdata {
    serviceDescription :: S.ByteString,
    serviceState       :: S.ByteString
} deriving (Show)

-- |The check type, either Service with associated ServiceData or Host.
data HostOrService = Service ServicePerfdata | Host deriving (Show)

-- |Encapsulates all the data in a check result that's relevant to 
-- metrics (we throw away things like the state type of HARD/SOFT). 
data Perfdata = Perfdata {
    dataType :: HostOrService,
    timestamp :: Int64,
    hostname :: S.ByteString,
    hostState :: S.ByteString,
    perfMetrics   :: MetricList
} deriving (Show)

type ParserError = [Char]

-- |Parse the output from a Nagios check.
parseLine :: S.ByteString -> Result [Item]
parseLine = parse line

fmtParseError :: [String] -> String -> String
fmtParseError ctxs err = concat $ err : "\n" : (intercalate "," ctxs) : []

-- |We have no more data to give the parser at this point, so we 
-- either fail or succeed here and return a ParserError or an ItemMap 
-- respectively. 
extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is) = Right $ mapItems is
extractItems (Fail _ ctxs err) = Left $ fmtParseError ctxs err
extractItems (Partial f) = extractItems (f "")

type MaybeError = Maybe ParserError

type MaybePerfdata = Maybe Perfdata

type ErrorState = State MaybeError

-- |Called if the check output is from a service check. Returns the 
-- service-specific component of the perfdata.
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

-- |Whether this perfdata item is for a host check or a service check 
-- (or Nothing on failure to determine). 
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

-- |Parse the component of the check output which contains the 
-- performance metrics (HOSTPERFDATA or SERVICEPERFDATA). 
parseMetricString :: S.ByteString -> Either ParserError MetricList
parseMetricString = completeParse . parse metricLine
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

-- |Given an item map extracted from a check result, parse and return 
-- the performance metrics (or store an error and return Nothing). 
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

-- |Given an item map extracted from a check result, parse and return 
-- a Perfdata object.
extractPerfdata :: ItemMap -> Either ParserError Perfdata
extractPerfdata m = case (extract m) of
    (_, Just err) -> Left err
    (Just res, Nothing)    -> Right res
    (Nothing, Nothing)     -> Left "Perfdata parser is buggy."
  where
    extract m = flip runState Nothing $ do 
        dType <- parseDataType m
        case dType of 
             Nothing -> return Nothing
             Just typ -> do
                 hName <- parseHostname m
                 case hName of
                     Nothing -> return Nothing
                     Just name -> do
                         tStamp <- parseTimestamp m
                         case tStamp of 
                             Nothing -> return Nothing
                             Just t -> do
                                 hState <- parseHostState m
                                 case hState of
                                     Nothing -> return Nothing
                                     Just state -> do
                                         pMetrics <- parseMetrics typ m
                                         case pMetrics of 
                                             Nothing -> return Nothing
                                             Just ms -> return $ Just $ Perfdata typ t name state ms

-- |Extract perfdata from a Nagios check result formatted according 
-- to the Nagios plugin development guidelines[0].
-- [0] https://nagios-plugins.org/doc/guidelines.html                                           
perfdataFromByteString :: S.ByteString -> Either ParserError Perfdata
perfdataFromByteString s = case (getItems s) of
    Left err -> Left err
    Right is -> extractPerfdata is
  where
    getItems = extractItems . parseLine
