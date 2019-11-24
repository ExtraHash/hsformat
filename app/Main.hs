import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import Data.Char (toUpper)
import Data.List (group)

main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName

    let result = formatFile contents



    let outputFileName = createOutputFileName fileName
    writeFile outputFileName result

createOutputFileName :: String -> String
createOutputFileName fileName = fileName ++ ".out.hs"

formatFile :: String -> String
formatFile contents = concatMap removeExcessNewlines (group contents)

removeExcessNewlines :: String -> String
removeExcessNewlines (x:xs) = if x == '\n' && length xs > 2 then "\n\n" else x:xs