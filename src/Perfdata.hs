{-# LANGUAGE OverloadedStrings #-}

module Perfdata where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.Word

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

