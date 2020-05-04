{-# LANGUAGE GADTs #-}
module DataStructures where
import qualified Data.Map as M

data Register where
    KBD :: Register
    SP :: Register
    LCL :: Register
    ARG :: Register
    THIS :: Register
    THAT :: Register
    R0 :: Register
    R1 :: Register
    R2 :: Register
    R3 :: Register
    R4 :: Register
    R5 :: Register
    R6 :: Register
    R7 :: Register
    R8 :: Register
    R9 :: Register
    R10 :: Register
    R11 :: Register
    R12 :: Register
    R13 :: Register
    R14 :: Register
    R15 :: Register
    CurrentSymbol :: Register
    CurLine :: Register
    User_String :: String -> Register
    deriving Show

data Dest where
    DestNull :: Dest --Represents no dest / 000
    M :: Dest
    D :: Dest
    MD :: Dest
    A :: Dest
    AM :: Dest
    AD :: Dest
    AMD :: Dest
    deriving Show

data Jump where
    JumpNull :: Jump -- Represents no jump / 000
    JGT :: Jump
    JEQ :: Jump
    JGE :: Jump
    JLT :: Jump
    JNE :: Jump
    JLE :: Jump
    JMP :: Jump
    deriving Show

data Comp where
    Zero :: Comp
    One :: Comp
    NegativeOne :: Comp
    D_Register :: Comp
    Address_Register :: Comp
    Not_D_Register :: Comp
    Not_Address_Register :: Comp
    Minus_D :: Comp 
    Minus_A :: Comp
    D_Plus_One :: Comp
    A_Plus_One :: Comp
    D_Minus_One :: Comp
    A_Minus_One :: Comp
    D_Plus_A :: Comp
    D_Minus_A :: Comp
    A_Minus_D :: Comp
    D_And_A :: Comp
    D_Or_A :: Comp
    M_Comp :: Comp
    Not_M :: Comp
    Minus_M :: Comp
    M_Plus_One :: Comp
    M_Minus_One :: Comp
    D_Plus_M :: Comp
    D_Minus_M :: Comp
    M_Minus_D :: Comp
    D_And_M :: Comp
    D_Or_M :: Comp    
    deriving Show


data Instruction where
    A_Inst :: Integer -> Instruction --The Leading Bit is only represented in the binary output.
    C_Instruction :: Comp -> Dest -> Jump -> Instruction -- The leading 3 bits are only in the binary output, not in the data structure
    deriving Show



--Now as a Map!
symbolTable = M.fromList([("KBD", 16384), ("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4), ("R0", 0), ("R1", 1), ("R2", 2), ("R3", 3), ("R4", 4), ("R5", 5), ("R6", 6), ("R7",7), ("R8", 8), ("R9", 9), ("R10", 10), ("R11", 11), ("R12", 12), ("R13", 13), ("R14", 14), ("R15", 15), ("currentSymbol", 16), ("curLine", 0)]) :: (M.Map String Integer)


registersToString :: Register -> String 
registersToString KBD = "KBD"
registersToString SP = "SP"
registersToString LCL = "LCL"
registersToString ARG = "ARG"
registersToString THIS = "THIS"
registersToString THAT = "THAT"
registersToString R0 = "R0"
registersToString R1 = "R1"
registersToString R2 = "R2"
registersToString R3 = "R3"
registersToString R4 = "R4"
registersToString R5 = "R5"
registersToString R6 = "R6"
registersToString R7 = "R7"
registersToString R8 = "R8"
registersToString R9 = "R9"
registersToString R10 = "R10"
registersToString R11 = "R11"
registersToString R12 = "R12"
registersToString R13 = "R13"
registersToString R14 = "R14"
registersToString R15 = "R15"
registersToString CurrentSymbol = "CurrentSymbol"
registersToString CurLine = "CurLine"
registersToString (User_String s) = s






