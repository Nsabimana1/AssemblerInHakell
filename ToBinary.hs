{-# LANGUAGE GADTs #-}

module ToBinary where

import           DataStructures
import           Numeric


--https://rosettacode.org/wiki/Binary_digits#Haskell
toBin :: Integer -> String
toBin n = showIntAtBase 2 ("01" !!) n ""

instructionToBinary :: Instruction -> String
instructionToBinary (A_Inst n) = "1" ++ padUntilFifteenBits (toBin n)
instructionToBinary (C_Instruction comp dest jump) ="111" ++ compToBinary comp ++ destToBinary dest ++ jmpToBinary jump

padUntilFifteenBits :: String -> String -- This pads out a string for use in A instructions.
padUntilFifteenBits n = strPadLeft '0' (fromIntegral (15 - length (n))) n

--This is roughly based on the code from https://hackage.haskell.org/package/strings-1.1/docs/Data-Strings.html
--I couldn't get the package to import, and I also wished to improve on the code, so I decided to rewrite it.
strPadLeft :: Char -> Integer -> String -> String
strPadLeft _ 0 right = right
strPadLeft paddingChar goUntil right = (strPadLeft paddingChar (goUntil - 1) ((replicate 1 paddingChar) ++ right))

destToBinary :: Dest -> String
destToBinary DestNull = "000"
destToBinary M        = "001"
destToBinary D        = "010"
destToBinary MD       = "011"
destToBinary A        = "100"
destToBinary AM       = "101"
destToBinary AD       = "110"
destToBinary AMD      = "111"

jmpToBinary :: Jump -> String --This just parses the Opcode
jmpToBinary JumpNull = "000"
jmpToBinary JGT      = "001"
jmpToBinary JEQ      = "010"
jmpToBinary JGE      = "011"
jmpToBinary JLT      = "100"
jmpToBinary JNE      = "101"
jmpToBinary JLE      = "110"
jmpToBinary JMP      = "111"

compToBinary :: Comp -> String
compToBinary Zero                 = "0101010"
compToBinary One                  = "0111111"
compToBinary NegativeOne          = "0111010"
compToBinary D_Register           = "0001100"
compToBinary Address_Register     = "0110000"
compToBinary Not_D_Register       = "0001101"
compToBinary Not_Address_Register = "0110001"
compToBinary Minus_D              = "0001111"
compToBinary Minus_A              = "0110011"
compToBinary D_Plus_One           = "0011111"
compToBinary A_Plus_One           = "0110111"
compToBinary D_Minus_One          = "0001110"
compToBinary A_Minus_One          = "0110010"
compToBinary D_Plus_A             = "0000010"
compToBinary D_Minus_A            = "0010011"
compToBinary A_Minus_D            = "0000111"
compToBinary D_And_A              = "0000000"
compToBinary D_Or_A               = "0010101"
compToBinary M_Comp               = "1110000"
compToBinary Not_M                = "1110001"
compToBinary Minus_M              = "1110011"
compToBinary M_Plus_One           = "1110111"
compToBinary M_Minus_One          = "1110010"
compToBinary D_Plus_M             = "1000010"
compToBinary D_Minus_M            = "1010011"
compToBinary M_Minus_D            = "1000111"
compToBinary D_And_M              = "1000000"
compToBinary D_Or_M               = "1010101"
