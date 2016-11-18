{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Record where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Internal.Types (Parser)

import qualified Data.ByteString.Char8 as C
import ParserBaseLib
import Lib

import Data.List (intercalate)
import Data.Char (chr)
import Data.Word8 (Word8)
import Data.List.Split (splitPlaces)

import Data.Either (isRight)

parse positions xs =
    let betterTry = better xs
        len = length xs

        fieldRunLengths = indexToDelta positions

        simple = splitPlaces $ fieldRunLengths

        _better = parseOnly $ do
            let ps = map rightPaddedData' $ fieldRunLengths
            sequence ps

        better = _better . C.pack
    in if isRight betterTry
        then betterTry
        else (Left $ show $ map bracketShowStr $ simple xs)
