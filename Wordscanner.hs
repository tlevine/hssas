module Wordscanner where

import qualified Text.ParserCombinators.Parsec as P
import           WordscannerHelpers           (name', literal',
                                               dateNumber, scientificNumber,
                                               specialCharacterChoices)

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
name = do x <- name'
          return $ Name x

literal :: P.Parser Word
literal = do x <- literal'
             return $ Literal x

number :: P.Parser Word
number = do x <- dateNumber P.<|> scientificNumber
            return $ Number x

specialCharacter :: P.Parser Word
specialCharacter = do x <- P.choice specialCharacterChoices
                      return $ SpecialCharacter x

word :: P.Parser Word
word = specialCharacter P.<|> number P.<|> literal P.<|> name

statement :: P.Parser [Word]
statement = do x <- P.sepBy word P.spaces
               P.spaces
               P.char ';'
               return x

p t = P.parse t "(unspecified source)"
