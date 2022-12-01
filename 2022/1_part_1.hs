import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read

groupElfs :: String -> [String]
groupElfs a = splitOn "\n\n" a

groupCallories :: [String] -> [[String]]
groupCallories (x:sx) = [(splitOn "\n" x)] ++ (groupCallories sx)
groupCallories [] = []

convertToNumbers :: [[String]] -> [[Int]]
convertToNumbers x = (map.map) (\a -> (fromMaybe 0 (readMaybe a :: Maybe Int))) x

sumGroup :: [[Int]] -> [Int]
sumGroup x = map sum x

getInput :: IO String
getInput = do done <- isEOF
              if done
              then return ""
              else do
                c <- getChar
                l <- getInput
                return (c:l)

main :: IO ()
main = do c <- getInput
          print (maximum (sumGroup (convertToNumbers (groupCallories (groupElfs c)))))

