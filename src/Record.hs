module Record where

import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.List.Split (splitPlaces)
import Data.Either (isRight)

import ParserBaseLib (rightPaddedData')
import Lib (bracketShowStr, indexToDelta)

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
