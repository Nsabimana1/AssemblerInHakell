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
                                writeArrayToFile outputFile (assembleMultiple lines)
    _ -> putStrLn "Usage: assembler <input file> <output file>"

charFound :: Char -> String -> Bool
charFound c s = any (== c) s

assemble :: String -> Maybe (String)
assemble s 
            | charFound '@' s                         = Just (instructionToBinary (fst (mHelper (runParser Parser.aInstParser s))))      
            | (charFound '=' s) || (charFound ';' s)  = Just(instructionToBinary (fst (mHelper (runParser Parser.cInstParser s))))
            | otherwise = Nothing

assembleMultiple :: [String] -> [String]
assembleMultiple (a:as) = case assemble a of 
                            Just b -> b : assembleMultiple as
                            Nothing -> assembleMultiple as
assembleMultiple [] = []