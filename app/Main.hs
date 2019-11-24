import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import Data.List (group)

main :: IO ()
main = do
    fileName <- head <$> getArgs

    contents <- lines <$> readFile fileName
    let removedWhitespaces = map trimWhitespaces contents
    let result = unlines removedWhitespaces

    let cleanResult = trimNewlines result
    let outputFileName = createOutputFileName fileName

    writeFile outputFileName cleanResult
    
createOutputFileName :: String -> String
createOutputFileName fileName = fileName ++ ".out.hs"

trimNewlines :: String -> String
trimNewlines contents = concatMap enforceTwoSequentialNewlineMaximum (group contents)

enforceTwoSequentialNewlineMaximum :: String -> String
enforceTwoSequentialNewlineMaximum (x:xs) = if x == '\n' && length xs > 2 then "\n\n" else x:xs

trimWhitespaces :: String -> String
trimWhitespaces line = if hasOnlyWhitespace line 
    then ""
    else line

hasOnlyWhitespace :: String -> Bool
hasOnlyWhitespace = all (`elem` ['\t', ' '])
