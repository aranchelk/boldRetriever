{-# LANGUAGE RecordWildCards #-}

module UnknownPage where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)

import qualified Data.Map.Strict as Map 
import Data.Either (rights)
import Data.List (sort)

import Data.Word8 

import ParserBaseLib
import Lib

----------------------------------------------------------------
-- Place holders for sections that cannot currently be parsed --
----------------------------------------------------------------

pageParser = do 
    _ <- word8 0
    typeId <- fmap fromIntegral anyWord8
    header <- count 14 anyWord8
    payload <- many1 anyWord8
    return $ Page {..}

data Page = Page { typeId :: Word8
            , header :: [Word8]
            , payload :: [Word8]
            }

instance Show Page where
    show (Page t h p) = unlines [ "\n***UNKNOWN***"
                             , (unwords ["typeId: ", (show t)])
                             , (unwords ["header: ", (show h)])
                             , (blockPrint 32 p)
                             ]
