{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Assembler where

import           DataStructures
import           Numeric
import           Parser
import           ToBinary
import qualified Data.Map as M
import           Prelude (any, foldl, foldr, fst, head)
import           AParser
import           Data.Bool
import           Data.Eq

import            Data.Char
import            Control.Applicative
import            Data.String

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