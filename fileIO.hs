import System.IO

main = do
  ls <- lines <$> readFile "testFile.txt"
  print ls
  print (someMethod ls)
  return ls

someMethod :: [String] -> Integer
someMethod a = 1
