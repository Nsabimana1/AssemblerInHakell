{-# LANGUAGE GADTs #-}
module DataStructures where
    

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

data Dest where
    DestNull :: Dest --Represents no dest / 000
    M :: Dest
    D :: Dest
    MD :: Dest
    A :: Dest
    AM :: Dest
    AD :: Dest
    AMD :: Dest

data Jump where
    JumpNull :: Jump -- Represents no jump / 000
    JGT :: Jump
    JEQ :: Jump
    JGE :: Jump
    JLT :: Jump
    JNE :: Jump
    JLE :: Jump
    JMP :: Jump

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




data Instruction where
    A_Inst :: Integer -> Instruction --The Leading Bit is only represented in the binary output.
    C_Instruction :: Comp -> Dest -> Jump -> Instruction -- The leading 3 bits are only in the binary output, not in the data structure




symbolTable :: Register -> Integer
symbolTable KBD = 16384
symbolTable SP = 0
symbolTable LCL = 1
symbolTable ARG = 2
symbolTable THIS = 3
symbolTable THAT = 4
symbolTable R0 = 0
symbolTable R1 = 1
symbolTable R2 = 2
symbolTable R3 = 3
symbolTable R4 = 4
symbolTable R5 = 5
symbolTable R6 = 6
symbolTable R7 = 7
symbolTable R8 = 8
symbolTable R9 = 9
symbolTable R10 = 10
symbolTable R11 = 11
symbolTable R12 = 12
symbolTable R13 = 13
symbolTable R14 = 14
symbolTable R15 = 15
symbolTable CurrentSymbol = 16
symbolTable CurLine = 0


