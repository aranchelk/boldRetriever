{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StartOfRecordIndex where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)

import ParserBaseLib

-- Start of record index (SRI) (68)
code = 68
isSRI = (word8 0) >> (int8 68)

pageParser = do
    isSRI
    count 5 anyWord8

    payloadRaw <- many' $ choice [recordParser, junkParser]
    let payload = validRecords payloadRaw
    many' $ anyWord8
    return $ map fst payload

recordParser = do
    addr <- anyInt16
    s <- anyInt8
    r <- int8 1
    r' <- count 3 anyInt8
    return $ ValidRecord ((addr, s), (r:r'))

junkParser = do
    r <- count 7 anyInt8
    return $ InvalidRecord r

validRecords = f' . filter f
    where
        f (ValidRecord _) = True
        f (InvalidRecord _) = False
        f' = map $ \(ValidRecord x) -> x

data Record = ValidRecord ((Int,Int), [Int])
                | InvalidRecord [Int] deriving (Show)
