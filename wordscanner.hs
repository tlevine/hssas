module Wordscanner where

import qualified Text.ParserCombinators.Parsec as P

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

literal' :: P.Parser String
literal' = do quoteChar <- P.oneOf "\"'"
              body <- P.many $ character quoteChar
              _ <- P.char quoteChar
              return body
  where
    character quoteChar = (P.noneOf [quoteChar]) P.<|>
                          (P.try (P.string [quoteChar, quoteChar] >> return quoteChar))

integerNumber :: P.Parser String
integerNumber = do valence <- P.option '+' $ P.char '-'
                   magnitude <- P.many P.digit
                   return $ valence:magnitude

decimalNumber :: P.Parser String
decimalNumber = do left <- integerNumber
                   middle <- P.char '.'
                   right <- P.many P.digit
                   return $ left ++ ( middle : right )

scientificNumber :: P.Parser String
scientificNumber = do left <- decimalNumber
                      middle <- P.char 'e'
                      right <- integerNumber
                      return $ left ++ ( middle : right )

dateNumber :: P.Parser String
dateNumber = do left <- literal'
                right <- P.char 'd'
                return $ left ++ [right]

number :: P.Parser String
number = dateNumber P.<|> scientificNumber P.<|> decimalNumber P.<|> integerNumber

p t = P.parse t "(unspecified source)"
