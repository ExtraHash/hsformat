import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import Data.List (group)

main :: IO ()
main = do
    fileName <- head <$> getArgs
    writeFile (fileName ++ ".out.hs") 
        =<<  stringOps
        . unlines
        . lineOps
        . lines
        <$> readFile fileName
        where
            lineOps 
                = trimLastNewlines 
                . map trimLastWhitespaces 
                . filter (not . isWhitespace)
            stringOps = trimNewlines
    

isWhitespace :: String -> Bool
isWhitespace [] = False
isWhitespace line = all (`elem` [' ', '\t']) line

trimNewlines :: String -> String
trimNewlines =
    concatMap enforceTwoSequentialNewlineMax . group

enforceTwoSequentialNewlineMax :: String -> String
enforceTwoSequentialNewlineMax ('\n':'\n':_) = "\n\n"
enforceTwoSequentialNewlineMax x = x

trimWhitespaceOnlyLines :: String -> String
trimWhitespaceOnlyLines line
    | isWhitespace line = ""
    | otherwise = line

trimLastWhitespaces :: String -> String
trimLastWhitespaces [] = []
trimLastWhitespaces line
    | last line == ' ' || last line == '\t' = 
        trimLastWhitespaces $ init line
    | otherwise = line

trimLastNewlines :: [String] -> [String]
trimLastNewlines [] = []
trimLastNewlines lines
    | last lines == "" = trimLastNewlines $ init lines
    | otherwise = lines

fixIndent :: String -> String
fixIndent line
    | rem (length line) 4 == 0 = line
    | otherwise = addLeadingWhitespace line 4

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace line
    | head line == ' ' = trimLeadingWhitespace $ tail line
    | otherwise = line

addLeadingWhitespace :: String -> Int -> String
addLeadingWhitespace line number
    | number > 0 = addLeadingWhitespace (' ':line) (number - 1)
    | otherwise = line
