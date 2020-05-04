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


-- *Assembler> instructionToBinary (C_Instruction D_Register DestNull JGT)      
-- "1110001100000001"
-- *Assembler> instructionToBinary (C_Instruction D_Register DestNull JGT)
-- "1110001100000001"
-- *Assembler> runParser Parser.cInstParser "D;JG"
-- Just (C_Instruction D_Register DestNull JumpNull,"")
-- *Assembler> m 

charFound :: Char -> String -> Bool
charFound c s = any (== c) s

-- assemble :: String -> String
-- assemble s = case (charFound '@' s) of 
--                 True -> instructionToBinary (fst t)
--                         where t = mHelper (runParser Parser.aInstParser s)
--                 False -> instructionToBinary (fst t)
--                         where t = mHelper (runParser Parser.cInstParser s)

assemble :: String -> String
assemble s 
            | charFound '@' s                         = instructionToBinary (fst (mHelper (runParser Parser.aInstParser s)))      
            | (charFound '=' s) || (charFound ';' s)  = instructionToBinary (fst (mHelper (runParser Parser.cInstParser s)))
            | (head s) == '/' = "/n"
            | (head s) == '(' = "/n"
            | otherwise = ""
