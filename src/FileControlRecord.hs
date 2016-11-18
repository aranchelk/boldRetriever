{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileControlRecord where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)
import Data.Word (Word8)

import qualified Data.ByteString as B

import Lib
import ParserBaseLib

isFCR = (char8 'F') >> (char8 'C')

-- Btrieve file control record contains metadata about the database file structure including page size.
-- Todo: actually parse this file.

pageParser = do
    isFCR
    payload <- many' anyWord8
    return $ Page {..}

data Page = Page { payload :: [Word8] }
instance Show Page where
    show p = unlines [ "***FCR***" 
                     , (unlines $ map (concatMap showBracket) $ splitEveryN 32 $ payload p)
                     ]
