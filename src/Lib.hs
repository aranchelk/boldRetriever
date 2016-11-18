module Lib ( diffDups
           , searchFor
           , sub
           , prettyPrintByteString
           , showBracket
           , blockPrint
           , bracketShowStr
           , elemsAtIndices
           , splitEveryN
           , splitEveryNBS
           , mergeLines
           , wrapBrackets
           , showUnreadableChr
           , indexToDelta) where


import Data.Char (isAlphaNum, isPrint, ord, chr)
import Data.List (foldl', splitAt)
import Data.Monoid
import Numeric (showHex, readHex)
import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word
import Data.Either (lefts, rights)

wrapBrackets x = concat ["[", x, "]"]

toDecString x = wrapBrackets dString
  where dString = show $ ord x

data ByteOrChar = BoCChar Char | BoCByte Char deriving (Eq)
 
instance Show ByteOrChar where
    show (BoCChar x) = x:[]
    show (BoCByte x) =  toDecString x

_indexToDelta [] = []
_indexToDelta (_:[]) = []
_indexToDelta (x1:x2:xs) = (x2 - x1) : _indexToDelta (x2:xs)

indexToDelta xs = _indexToDelta paddedXs
    where paddedXs = 0:xs


_shouldShowAsChar whiteList blackList x = x `elem` whiteList && (not $ x `elem` blackList)

shouldShowAsChar = _shouldShowAsChar [32 .. 126] [91, 93]
shouldShowChr x = shouldShowAsChar $ fromIntegral $ ord x

bracketPrintChr x =
    if shouldShowChr x
        then x:[]
        else "[" ++ (show(ord x)) ++ "]"

showUnreadableChr x =
    case x of
        '\0' -> '_'
        ' ' -> '-'
        '_' -> '-'
        _ -> '*'

bracketShowStr xs = concatMap bracketPrintChr xs

numToChrString x = (chr (fromIntegral x :: Int)) : []
showBracket x = concat $ ["[", (show x), "]"]

word8AsciiShow x =
    if shouldShowAsChar x
        then numToChrString x
        else showBracket x

prettyPrintWord8 :: [Word8] -> String
prettyPrintWord8 = concatMap word8AsciiShow 

prettyPrintByteString = prettyPrintWord8 . B.unpack

splitEveryN :: Int -> [a] -> [[a]]
splitEveryN _ [] = []
splitEveryN n xs = nXs : (splitEveryN n rXs)
    where (nXs, rXs) = splitAt n xs


splitEveryNBS :: Int -> B.ByteString -> [B.ByteString]
splitEveryNBS n xbs
    | (B.length xbs) <= n = [xbs]
    | otherwise = x : splitEveryNBS n xs
        where (x, xs) = B.splitAt n xbs

-- Todo: make this not suck
elemsAtIndices :: [a] -> [Int] -> [a]
elemsAtIndices xs inds = foldl f [] inds
    where f acc i = acc ++ [(xs !! i )]

diffDups [] = []
diffDups (_:[]) = []
diffDups ((x, xn):(y, yn):xs) = ((y - x), xn):(diffDups ((y, yn):xs))

sub xs n 
    | n < 0 = e
    | (n + 1) > (length xs) = e
    | otherwise = Right $ xs !! n
    where e = Left ("Index out of range! " ++ (show ((length xs), n)))

searchFor kvs k = f r
    where
        r = lookup k kvs
        f Nothing = Left "Lookup failed to find key"
        f (Just v) = Right v

blockPrint n = unlines . map prettyPrintWord8 . (splitEveryN n)

_overwriteChar newIsBetter (o, n) =
    if newIsBetter o n
        then n
        else o

_nib '\0' _ = True
_nib _ '\0' = False
_nib _ _ = True

zipWithPadding _ [] [] = []
zipWithPadding p (x:xs) (y:ys) = (x,y) : zipWithPadding p xs ys
zipWithPadding p (x:xs) [] = (x,p) : zipWithPadding p xs [] 
zipWithPadding p [] (y:ys) = (p,y) : zipWithPadding p [] ys

_overwriteIf f p old new =
    let oN = zipWithPadding p old new
        pred = _overwriteChar f
        in map pred oN


mergeLines (l:ls) = foldl' (_overwriteIf _nib '\0') l ls
