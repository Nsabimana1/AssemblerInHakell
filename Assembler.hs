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
destParser = (seqA (map char "AMD") *> pure AMD) <|> (seqA (map char "MD") *> pure MD) <|> (seqA (map char "AM") *> pure AM) <|> (seqA (map char "AD") *> pure AD) <|> (char ' ' *> pure DestNull) <|> (char 'M' *> pure M) <|>  (char 'D' *> pure D) <|>  (char 'A' *> pure A) 

-- jumpParser :: Parser Jump
-- jumpParser = (char ' ' *> pure JumpNull) <|> (char 'JGT' *> pure JGT) <|> (char 'JEQ' *> pure 'JEQ') <|>(char 'JGE' *> pure JGE) <|>(char 'JLT' *> pure JLT) <|>(char 'JNE' *> pure JNE) <|>(char 'JLE' *> pure JLE) <|> (char 'JMP' *> pure JMP)

-- To tese for aInstParser
-- runParser aInstParser_  "@ABC"

-- To test for cInstparser
-- runParser cInstParser_  "AB=C"

