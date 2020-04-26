{-# LANGUAGE GADTs #-}

module ToBinary where
import DataStructures


destToBinary :: Dest -> String
destToBinary DestNull = "000"
destToBinary M = "001"
destToBinary D= "010"
destToBinary MD = "011"
destToBinary A = "100"
destToBinary AM = "101"
destToBinary AD = "110"
destToBinary AMD = "111"

jmpOpcodeToBinary :: Jump -> String --This just parses the Opcode
jmpOpcodeToBinary JumpNull = "000"
jmpOpcodeToBinary JGT = "001"
jmpOpcodeToBinary JEQ = "010"
jmpOpcodeToBinary JGE = "011"
jmpOpcodeToBinary JLT = "100"
jmpOpcodeToBinary JNE = "101"
jmpOpcodeToBinary JLE = "110"
jmpOpcodeToBinary JMP = "111"

jmpToBinary :: Jump -> Dest -> String
jmpToBinary jmp dest = (jmpOpcodeToBinary jmp) ++ (destToBinary dest)

compToBinary :: Comp -> String
compToBinary Zero = "0101010"
compToBinary One = "0111111"
compToBinary NegativeOne = "0111010"
compToBinary D_Register = "0001100"
compToBinary Address_Register = "0110000"
compToBinary Not_D_Register= "0001101"
compToBinary Not_Address_Register = "0110001"
compToBinary Minus_D = "0001111"
compToBinary Minus_A = "0110011"
compToBinary D_Plus_One = "0011111"
compToBinary A_Plus_One = "0110111"
compToBinary D_Minus_One = "0001110"
compToBinary A_Minus_One = "0110010"
compToBinary D_Plus_A = "0000010"
compToBinary D_Minus_A = "0010011"
compToBinary A_Minus_D = "0000111"
compToBinary D_And_A = "0000000"
compToBinary D_Or_A = "0010101"
compToBinary M_Comp = "1110000"
compToBinary Not_M = "1110001"
compToBinary Minus_M = "1110011"
compToBinary M_Plus_One = "1110111"
compToBinary M_Minus_One = "1110010"
compToBinary D_Plus_M = "1000010"
compToBinary D_Minus_M = "1010011"
compToBinary M_Minus_D = "1000111"
compToBinary D_And_M = "1000000"
compToBinary D_Or_M = "1010101"
