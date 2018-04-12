import Data.String
import Data.List
import System.Environment
import System.IO
import Debug.Trace

splitFileContent  :: String -> [String]
splitFileContent content = lines content

stripNewlines :: [String] -> [String]
stripNewlines stringList = map (filter (/= '\r')) stringList

matchWord :: [String] -> String -> [String]
matchWord stringList pattern = filter (matchesPattern pattern) stringList


matchesPattern :: String -> String -> Bool
--matchesPattern a b | trace ("myfun " ++ show a ++ " " ++ show b) False = undefined
matchesPattern [] [] = True
matchesPattern [] pattern = False
matchesPattern word [] = False
matchesPattern (patternHead:patternRest) (wordHead:wordRest)
  | patternHead == '_' = matchesPattern patternRest wordRest
  | patternHead /= wordHead = False
  | otherwise = matchesPattern patternRest wordRest


main :: IO()
main = do
  [filePath, pattern] <- getArgs
  fileContent <- readFile filePath
  let words = stripNewlines(splitFileContent(fileContent))
  -- mapM_ putStrLn someWords
  let x = pattern
  -- mapM_ putStrLn (matchWord ["soki"] x)
  mapM_ putStrLn (matchWord words x)
