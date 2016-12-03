{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PageAllocationTable where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)

import qualified Data.Map.Strict as Map 
import Data.Either (rights)
import Data.List (sort)

import ParserBaseLib
import Lib

---------------------------
-- Page allocation table --
---------------------------
isPAT = (word8 0) >> (word8 80)

pageParser = do
    isPAT
    pagePairNum <- anyInt8
    anyWord8
    pageUsageCount <- anyInt8
    count 3 anyWord8
    pageEntries <- many' entryParser
    return $ Page {..} 

filterOldPATs xs = Map.elems $ foldl fop (Map.empty) xs
    where 
        fop kv p = Map.insertWith (\new old ->  if (pageUsageCount old) > (pageUsageCount new)
                                                    then old
                                                    else new
                                  ) (pagePairNum p) p kv


-- Btrieve page data parsing --
data Page = Page { pagePairNum :: Int -- Starts at 1, and increments for each set of PATs
                          , pageUsageCount :: Int -- Higher number determines the active PAT
                          , pageEntries :: [(Int, Int)] -- (peTypeNumber, pePagePos  
                          }

instance Show Page where
    show (Page ppn puc pes) = unlines [ "\n***PAT***"
                               , "pagePairNum:" ++ (show $ ppn)
                               , "patUsageCount:" ++ (show $ puc)
                               , (unlines $ map show $ pes)
                               ]


summarizePage p = concat [ "pnum: "
                        , (show $ pagePairNum p)
                        , ", "
                        , "pusage: "
                        , (show $ pageUsageCount p)
                        , "; "
                        ]

entryParser = do
    _ <- word8 0
    peTypeNumber :: Int <- fmap fromIntegral anyWord8
    posX1 :: Int <- fmap fromIntegral anyWord8
    posX256 :: Int <- fmap fromIntegral anyWord8
    let pos = posX256 * 256 + posX1
    return (peTypeNumber, pos)

getEntries pgs = let
    rawPATs = filterByParseSuccess isPAT pgs
    parsedPATs = rights $ mapParse pageParser rawPATs
    newestPATs = filterOldPATs parsedPATs
    in concatMap pageEntries newestPATs 

getEntriesWithCode all code = all `elemsAtIndices` indices
    where 
        code' = fromIntegral code
        indices = sort $ map snd $ filter (\x -> (fst x) == code') $ getEntries all



