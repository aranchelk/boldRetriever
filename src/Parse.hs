import System.Environment (getArgs) 

import qualified Data.ByteString as B

import Data.Either (rights)
import Data.String.Utils (replace)

import qualified PageAllocationTable as PAT
import qualified VariableDataPage as VDP
import qualified StartOfRecordIndex as SRI
import qualified Record as R
import qualified RunLengthEncodedBlock as RLB 

import Text.CSV (printCSV)

import ParserBaseLib (mapParse, concatMapParse, mapParse, mapParse)
import Lib (splitEveryNBS)

btPageLength = 4096

data CharType = NullChar
              | NonNullChar
instance Show CharType where
    show NullChar = "_"
    show NonNullChar = "A"

toCharType '\0' = NullChar
toCharType _ = NonNullChar

_findBoundaries i acc (NullChar:NonNullChar:xs) = _findBoundaries (i + 1) ((i + 1):acc) (NonNullChar:xs)
_findBoundaries i acc (_:xs) = _findBoundaries (i + 1) acc xs
_findBoundaries _ acc [] = acc

findBoundaries = reverse . _findBoundaries 0 []

-- Todo: Better handling of Lefts, save bad results, or at least count.
main :: IO ()
main = do
  args <- getArgs
  raw <- B.readFile $ head args

  let pos = read $ args !! 1

  let allPages = splitEveryNBS btPageLength raw

  let getPagesByCode = PAT.getEntriesWithCode allPages 

  let dataPages = rights $ mapParse VDP.pageParser $ getPagesByCode VDP.code
  let recordStartAddrs = concatMapParse SRI.pageParser $ getPagesByCode SRI.code

  let getRecord = VDP.getRecord dataPages
  let defraggedRaw = rights $ map getRecord recordStartAddrs

  let records = map concat $ rights $ mapParse RLB.blocks defraggedRaw

  -- Parsed output -- 
  let recordText = map concat $ rights $ mapParse RLB.blocks defraggedRaw
  let records = rights $ map (R.parse pos) recordText 
  putStr $ printCSV records
