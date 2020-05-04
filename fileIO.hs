import System.IO
import System.Environment

import Assembler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do 
                                lines <- lines <$> readFile inputFile
                                writeArrayToFile outputFile (removeEmptyLines(assembleMultiple lines))
    _ -> putStrLn "Usage: assembler <input file> <output file>"

removeEmptyLines :: [String] -> [String]
removeEmptyLines s = filter (\a -> not (a=="")) s

--https://stackoverflow.com/a/18711075
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

--This is what we actually want to use when writting our assembled code to the file!!!
writeArrayToFile :: FilePath -> [String] -> IO ()
writeArrayToFile filePath array = writeLines filePath (unlines array)

--https://stackoverflow.com/questions/2895748/how-to-write-list-to-file
writeLines :: FilePath -> String -> IO ()
writeLines filePath contents = do
    outh <- openFile filePath WriteMode
    hPutStr outh contents
    hClose outh