module ParserBaseLib where

import Data.Attoparsec.ByteString (word8, notWord8, anyWord8, parseOnly)
import Data.Attoparsec.Combinator (choice, count, many', lookAhead)
import Data.Attoparsec.ByteString.Char8 (char, notChar, anyChar)
import Data.Attoparsec.Internal.Types (Parser)
import Data.Either (isRight, rights)

import Lib

anyInt8 = fmap fromIntegral anyWord8
int8 x = fmap fromIntegral $ word8 $ fromIntegral x 

anyInt16 = do
    x1 <- anyInt8
    x256 <- anyInt8
    return $ (x1 + x256 * 256 :: Int)

anyEscapedChar = choice [escapedQuote, anyChar]

escapedQuote = do
    char '\\'
    char '"'

parserNOP = lookAhead anyChar 

limitMaxLen rl p = do
    d <- lookAhead p
    let dLen = length d
    if dLen > rl
        then fail "Consumed too many characters"
        else count dLen anyWord8
    return d

rightPaddedData rl = do
    d <- limitMaxLen rl $ many' nonNullChr
    count (rl - length d) anyChar
    return d

rightPaddedData' rl = choice [(rightPaddedData rl), (count rl anyChar)]

oneNull = word8 0

nonNulls = many' $ notWord8 0

nonNullChr = notChar '\0'

-- Maybe get rid of this stuff.
theRest = do
    t <- binData
    _ <- count 1 $ notWord8 0
    return $ show t

binData = do
    c <- count 1 anyWord8
    return $ head c

parseFilterPredicate p x = isRight $ parseOnly p x

filterByParseSuccess p xs = filter (parseFilterPredicate p) xs

mapParse p xs = map (parseOnly p) xs

mapParseAlt parsers xs = map (parseAlt parsers) xs

data ParserResult a = ParserResult { prMessages :: [String]
                                   , prPayload :: Maybe a
                                   } deriving (Show)

_parseAlt messages [] _ = ParserResult ("All parsers failed":messages) Nothing
_parseAlt messages (p:ps) x =
    let res = parseOnly p x
        f (Right r) = ParserResult messages $ Just r
        f (Left l) = _parseAlt (l:messages) ps x
    in f res
parseAlt = _parseAlt []

concatMapParse p xs = concat $ rights $ mapParse p xs
