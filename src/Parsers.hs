module Parsers 
    ( dbpSections
    ) where

import qualified PageAllocationTable as PAT
import Data.Attoparsec.ByteString (many1, choice, anyWord8)
import Data.Word8

import qualified FileControlRecord as FCR
import qualified PageAllocationTable as PAT
import qualified StartOfRecordIndex as SRI
import qualified VariableDataPage as VDP
import qualified UnknownPage as UNK

-- Todo: Try generating with Template Haskell, or creating typeclass with instances toSection
dbpSections = choice [ (fmap FCR FCR.pageParser )
                     , (fmap PAT PAT.pageParser)
                     , (fmap SRI SRI.pageParser)
                     , (fmap VDP VDP.pageParser)
                     , (fmap UNK UNK.pageParser)
                     , (fmap Unstructured $ many1 anyWord8) 
                     ]

data Section = FCR FCR.Page
             | PAT PAT.Page
             | SRI [(Int,Int)]
             | VDP VDP.Page
             | UNK UNK.Page
             | Unstructured [Word8] deriving (Show)
