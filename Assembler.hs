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

import            AParser

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