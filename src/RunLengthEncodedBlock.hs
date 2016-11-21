module RunLengthEncodedBlock where

import Data.Attoparsec.ByteString (count, many1)
import Data.Attoparsec.ByteString.Char8 (anyChar)
import ParserBaseLib (anyInt16)

block = do
    readLength <- anyInt16
    payload <- count readLength anyChar

    runLength <- anyInt16
    toExpand <- anyChar
    let expanded = replicate runLength toExpand
    return $ payload ++ expanded

blocks = many1 block 
