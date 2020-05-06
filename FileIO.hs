module FileIO where
import System.IO
import System.Environment



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