{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}

import AParser
import DataStructures
import                      Data.Char
import                      Control.Applicative
import                      Data.String

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
oneOrMore p = fmap(\s ss -> s: ss) p <*> DataStructures.zeroOrMore p 

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (DataStructures.oneOrMore p) <|> pure []

aInstParser_ :: Parser ()
aInstParser_ = fmap (\_ -> ()) (char '@')

stringParser :: Parser [Char]
stringParser = fmap DataStructures.oneOrMore satisfy(isAlpha)

cInstParser_ :: Parser ([Char],[Char])
cInstParser_ = fmap(\s1 -> \s2 -> (s1,s2)) (DataStructures.stringParser) <*> (((char '=') <|> (char ';')) *> (DataStructures.stringParser)) 

--Sort the operators based on precedence
destParser :: Parser Dest
destParser = (seqA (map char "AMD") *> pure AMD) <|> (seqA (map char "MD") *> pure MD) <|> (seqA (map char "AM") *> pure AM) <|> (seqA (map char "AD") *> pure AD) <|> (char 'M' *> pure M) <|>  (char 'D' *> pure D) <|>  (char 'A' *> pure A)  <|> pure DestNull --Haskell doesn't have null characters

jumpParser :: Parser Jump
jumpParser = (seqA(map char "JGT") *> pure JGT) <|> (seqA(map char "JEQ") *> pure JEQ) <|>(seqA(map char "JGE") *> pure JGE) <|>(seqA(map char "JLT") *> pure JLT) <|>(seqA(map char "JNE") *> pure JNE) <|>(seqA(map char "JLE") *> pure JLE) <|> (seqA(map char "JMP") *> pure JMP) <|> pure JumpNull -- Haskell doesn't have null characters 

-- To tese for aInstParser
-- runParser aInstParser_  "@ABC"

-- To test for cInstparser
-- runParser cInstParser_  "AB=C"

