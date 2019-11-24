import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import Data.List (group)

main :: IO ()
main = do
    fileName <- head <$> getArgs
    writeFile (fileName ++ "-out.hs")
        =<<  stringOps
        . unlines
        . lineOps
        . lines
        <$> readFile fileName
        where
            lineOps = map trimLastWhitespaces . filter (not . isWhitespace)
            stringOps = trimNewlines

isWhitespace :: String -> Bool
isWhitespace [] = False
isWhitespace line = all (`elem` [' ', '\t']) line

trimNewlines :: String -> String
trimNewlines =
    concatMap enforceTwoSequentialNewlineMax . group

enforceTwoSequentialNewlineMax :: String -> String
enforceTwoSequentialNewlineMax (x:xs) =
    if x == '\n' && length xs > 2
    then "\n\n"
    else x:xs

trimWhitespaceOnlyLines :: String -> String
trimWhitespaceOnlyLines line =
    if isWhitespace line
    then ""
    else line

trimLastWhitespaces :: String -> String
trimLastWhitespaces [] = []
trimLastWhitespaces line =
    if last line == ' '
    || last line == '\t'
    then trimLastWhitespaces $ init line
    else line
