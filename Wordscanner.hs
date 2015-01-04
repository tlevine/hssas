module Wordscanner where

import qualified Text.ParserCombinators.Parsec as P
import           WordscannerHelpers (literal', dateNumber, scientificNumber)

data Word = Name String |
            Literal String |
            Number String |
            SpecialCharacter String

showWord a b = a ++ " (" ++ b ++ ")"
instance Show Word where
  show (Name text) = showWord "Name" text
  show (Literal text) = showWord "Literal" text
  show (Number text) = showWord "Number" text
  show (SpecialCharacter text) = showWord "Special" text

name :: P.Parser Word
name = do a <- nameHead
          b <- nameTail
          return $ Name $ a:b
  where
    nameHead = P.letter P.<|> (P.char '_')
    nameTail = P.many $ nameHead P.<|> P.digit

literal :: P.Parser Word
literal = do x <- literal'
             return $ Literal x

number :: P.Parser Word
number = do x <- dateNumber P.<|> scientificNumber
            return $ Number x

p t = P.parse t "(unspecified source)"
