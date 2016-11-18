{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VariableDataPage where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)
import Data.Word (Word8)

import Data.Bifunctor (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Lib
import ParserBaseLib

-- Btrieve variable data pages contain record fragments
-- These fragements can be either:
--  * An entire record that doesn't exceed max fragment length
--  * The start of a record that exceeds max fragment length
--  * A middle or final part of a record that exceeds max fragment length
code = 86
isVDP = (word8 0) >> (word8 86)


pageParser = do
    isVDP
    pageAddress <- anyInt16
    pageUnkownHeaderFields <- count 6 anyInt8
    footerRecordCount :: Int <- anyInt16
    let pageRecordCount = footerRecordCount

    pageIndices <- fmap dedupDecode $ pageFooterParser footerRecordCount
    let recordLengths = diffDups pageIndices
    let recordParsers = map recordFragmentParser' recordLengths

    pagePayload <- fmap concat $ sequence recordParsers

    return $ Page {..}


pageFooterParser numOfRec = do
    let footerLen = (numOfRec + 1) * 2
    fullLength <- fmap length $ lookAhead $ many1 anyWord8
    rawF <- lookAhead $ do
        count (fullLength - footerLen) anyWord8
        many' anyInt16
    return $ reverse rawF


data Page = Page { pageAddress :: Int
                 , pageUnkownHeaderFields :: [Int]
                 , pageRecordCount :: Int
                 , pagePayload :: [RecordFragment]
                 , pageIndices :: [(Int, Int)]
                 }
instance Show Page where
    show p = unlines [ "***VariableDataPage***" 
                     , (unwords ["addr: ", (show $ pageAddress p)])
                     , (unwords ["recordCount: ", (show $ pageRecordCount p)])
                     , (unwords ["indices: ", (show $ pageIndices p)])
                     , (unwords ["unknownHeaderData: ", (show $ pageUnkownHeaderFields p)])
                     , (unlines $ map show $ pagePayload p)
                     ]

btrAddr = do
    -- Todo: addresses should be Int16
    -- Todo: This is a shared type, need a shared base lib
    a1 <- anyInt8
    a2 <- anyInt8
    return (a1, a2)



dedupDecode = dd 1
    where
        dd _ [] = []
        dd n (x:xs)
            | (isX255_255 x) = dd (n + 1) xs
            | otherwise = (x, n) : dd 1 xs


data RecordFragment = RecordFragment { rfNextFragAddress :: Maybe (Int, Int)
                                     , rfPayload :: B.ByteString
                                     }
instance Show RecordFragment where
    show r = unlines [ "*** Record Fragment ***"
                     , (unwords ["Next fragment:", (show $ rfNextFragAddress r)])
                     , "Payload:"
                     , (prettyPrintByteString $ rfPayload r)
                     ]


isX255_255 :: Int -> Bool
isX255_255 = (== (255 * 256 + 255))


rfAddrParser = do
    a <- count 4 anyInt8
    let addr = (a!!1) + (a!!2) * 256
    return $ case a of
        (255:255:255:255:[]) -> Nothing
        (_:a2:a3:a4:[]) -> Just (addr,a4)
  
recordFragmentParser rl = do
    rfNextFragAddress <- rfAddrParser
    rfPayload <- fmap B.pack $ count (rl - 4) anyWord8
    return $ RecordFragment {..}

recordFragmentParser' (rl, rep) = do
    r <- recordFragmentParser rl
    return $ replicate rep r

-- Todo: Clean up these 5 functions
zipVDPAddr = map $ \v -> ((pageAddress v), v)

getRecord_ vdps rAddr = do
    r <- vdps `getRecordFrag` rAddr
    let a = rfNextFragAddress r
    let p = rfPayload r

    p' <- case a of
              Nothing -> Right []
              (Just x) -> getRecord_ vdps x

    return $ p:p'

getRecord v a = second B.concat $ getRecord_ v a
    
getRecordFrag vdps (addr,s) = do
    p <- getVDP vdps addr
    let p' = pagePayload p
    p' `sub` s 

getVDP vdps addr = do
    let vdpAList = zipVDPAddr vdps
    vdpAList `searchFor` addr
