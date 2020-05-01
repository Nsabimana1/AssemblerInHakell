import System.IO

main = do
  ls <- lines <$> readFile "testFile.txt"
  print ls
  return ls
