{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}

import AParser

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
oneOrMore p = fmap(\s ss -> s: ss) p <*> zeroOrMore p 

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

aInstParser_ :: Parser ()
aInstParser_ = fmap (\_ -> ()) (char '@')

stringParser :: Parser [Char]
stringParser = fmap oneOrMore satisfy(isAlpha)

cInstParser_ :: Parser ([Char],[Char])
cInstParser_ = fmap(\s1 -> \s2 -> (s1,s2)) (stringParser) <*> (((char '=') <|> (char ';')) *> (stringParser)) 

-- To tese for aInstParser
-- runParser aInstParser_  "@ABC"

-- To test for cInstparser
-- runParser cInstParser_  "AB=C"

