{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- module Assembler where


import           Parser
import           ToBinary
import           Prelude 
import           AParser

import FileIO
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do 
                                lines <- lines <$> readFile inputFile
                                writeArrayToFile outputFile (removeEmptyLines(assembleMultiple lines))
    _ -> putStrLn "Usage: assembler <input file> <output file>"

charFound :: Char -> String -> Bool
charFound c s = any (== c) s


assemble :: String -> String
assemble s 
            | charFound '@' s                         = instructionToBinary (fst (mHelper (runParser Parser.aInstParser s)))      
            | (charFound '=' s) || (charFound ';' s)  = instructionToBinary (fst (mHelper (runParser Parser.cInstParser s)))
            | (head s) == '/' = ""
            | (head s) == '(' = ""
            | otherwise = ""

assembleMultiple :: [String] -> [String]
assembleMultiple (a:as) = assemble a : assembleMultiple as
assembleMultiple [] = []