{-# LANGUAGE OverloadedStrings #-}

module Perfdata where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
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

mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (name i) (value i) m) M.empty

data ServicePerfdata = ServicePerfData {
    serviceDescription :: S.ByteString,
    serviceState       :: S.ByteString,
    serviceStateType       :: S.ByteString
}

data HostOrService = Service ServicePerfdata | Host

data Perfdatum = Perfdatum {
    dataType :: HostOrService,
    timestamp :: Word64,
    hostname :: S.ByteString,
    hostState :: S.ByteString,
    hostStateType :: S.ByteString,
    perfString    :: S.ByteString
}

type ParserError = [Char]

parseLine :: S.ByteString -> Result [Item]
parseLine = parse line

extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is) = Right $ mapItems is
extractItems (Fail _ ctxs err) = Left $ concat $ err : "\n" : (intercalate "," ctxs) : []
extractItems (Partial f) = extractItems (f "")
