module RunLengthEncodedBlock where

import Data.Attoparsec.ByteString hiding (take)
import Data.Attoparsec.ByteString.Char8 (anyChar)

import ParserBaseLib

block = do
    readLength <- anyInt16
    payload <- count readLength anyChar

    runLength <- fmap fromIntegral anyInt16
    toExpand <- anyChar
    let expanded = replicate runLength toExpand
    return $ payload ++ expanded

blocks = many1 block 
