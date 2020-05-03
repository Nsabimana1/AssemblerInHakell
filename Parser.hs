{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Parser where
import           AParser
import           Control.Applicative
import           Data.Char
import qualified Data.Map as M

import           DataStructures

-- // This file is part of www.nand2tetris.org
-- // and the book "The Elements of Computing Systems"
-- // by Nisan and Schocken, MIT Press.
-- // File name: projects/06/add/Add.asm
-- // Computes R0 = 2 + 3  (R0 refers to RAM[0])
-- @2
-- D=A
-- @3
-- D=D+A
-- @0
-- M=D
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = fmap (\s ss -> s : ss) p <*> zeroOrMore p

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

aInstParser :: Parser Instruction
aInstParser = removesymbol *> ((fmap(\s -> A_Inst s) (posInt)) <|> (fmap(\s -> A_Inst (parsemaybehelper (M.lookup s symbolTable))) (Parser.stringParser)))

stringParser :: Parser [Char]
stringParser = fmap oneOrMore satisfy (isAlpha)

-- cInstParser_ :: Parser ([Char], [Char])
-- cInstParser_ =
--   fmap (\s1 -> \s2 -> (s1, s2)) (DataStructures.stringParser) <*> (((char '=') <|> (char ';')) *> (DataStructures.stringParser))

-- cInstParser :: Parser Instruction
-- cInstParser = (\dest comp -> C_Instruction(comp dest JumpNull))<$>destParser <*> (char '=' *> compParser)
-- cInstParser = (C_Instruction <$> compParser <*> (char '=' *> destParser) <*> pure JumpNull)
-- cInstParser =  ((\dest comp -> C_Instruction(comp dest JumpNull))) <$> destParser <*> (char '=' *> compParser) <*> pure JumpNull

--Sort the operators based on precedence
destParser :: Parser Dest
destParser =
  (seqA (map char "AMD") *> pure AMD) <|> (seqA (map char "MD") *> pure MD) <|>
  (seqA (map char "AM") *> pure AM) <|>
  (seqA (map char "AD") *> pure AD) <|>
  (char 'M' *> pure M) <|>
  (char 'D' *> pure D) <|>
  (char 'A' *> pure A) <|>
  pure DestNull -- Fallback of null character, since Haskell doesn't support empty strings.

jumpParser :: Parser Jump
jumpParser =
  (seqA (map char "JGT") *> pure JGT) <|> (seqA (map char "JEQ") *> pure JEQ) <|>
  (seqA (map char "JGE") *> pure JGE) <|>
  (seqA (map char "JLT") *> pure JLT) <|>
  (seqA (map char "JNE") *> pure JNE) <|>
  (seqA (map char "JLE") *> pure JLE) <|>
  (seqA (map char "JMP") *> pure JMP) <|>
  pure JumpNull -- Fallback of null character, since Haskell doesn't support empty strings.

compParser :: Parser Comp
compParser =
  (seqA (map char "D+1") *> pure D_Plus_One) <|>
  (seqA (map char "A+1") *> pure A_Plus_One) <|>
  (seqA (map char "D-1") *> pure D_Minus_One) <|>
  (seqA (map char "A-1") *> pure A_Minus_One) <|>
  (seqA (map char "D+A") *> pure D_Plus_A) <|>
  (seqA (map char "D-A") *> pure D_Minus_A) <|>
  (seqA (map char "A-D") *> pure A_Minus_D) <|>
  (seqA (map char "D&A") *> pure D_And_A) <|>
  (seqA (map char "D|A") *> pure D_Or_A) <|>
  (seqA (map char "M+1") *> pure M_Plus_One) <|>
  (seqA (map char "M-1") *> pure M_Minus_One) <|>
  (seqA (map char "D+M") *> pure D_Plus_M) <|>
  (seqA (map char "D-M") *> pure D_Minus_M) <|>
  (seqA (map char "M-D") *> pure M_Minus_D) <|>
  (seqA (map char "D&M") *> pure D_And_M) <|>
  (seqA (map char "D|M") *> pure D_Or_M) <|> --Three characters

  (seqA (map char "-1") *> pure NegativeOne) <|> --Two characters
  (seqA (map char "!D") *> pure Not_D_Register) <|>
  (seqA (map char "!A") *> pure Not_Address_Register) <|>
  (seqA (map char "-D") *> pure Minus_D) <|>
  (seqA (map char "-A") *> pure Minus_A) <|>
  (seqA (map char "!M") *> pure Not_M) <|>
  (seqA (map char "-M") *> pure Minus_M) <|>

  (char '1' *> pure One) <|> --One character
  (char 'D' *> pure D_Register) <|>
  (char 'A' *> pure Address_Register) <|>
  (char 'M' *> pure M_Comp) <|>
  (char '0' *> pure Zero) <|>


  pure Zero -- Fallback of null character, since Haskell doesn't support empty strings.

-- To tese for aInstParser
-- runParser aInstParser_  "@ABC"
-- To test for cInstparser
-- runParser cInstParser_  "AB=C"

removesymbol :: Parser String
removesymbol = zeroOrMore (satisfy (== '@'))

parsemaybehelper :: Maybe a -> a
parsemaybehelper (Just a) = a
