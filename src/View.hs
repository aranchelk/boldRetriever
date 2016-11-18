import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as B
import System.Environment (getArgs) 
import Lib (splitEveryNBS)
import Parsers (dbpSections)

btPageLength = 4096

main :: IO ()
main = do
    args <- getArgs

    putStrLn "*** Starting ***"
    let filePath = args !! 0
    raw <- B.readFile $ filePath

    let allPages = splitEveryNBS btPageLength raw
    let res = map (parseOnly dbpSections) allPages 

    putStrLn $ unlines $ map show res

    putStrLn "*** Done ***"
