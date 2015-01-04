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

wholeNumber :: P.Parser String
wholeNumber = P.many1 P.digit

integerNumber :: P.Parser String
integerNumber = do valence <- P.option '+' $ P.char '-'
                   magnitude <- wholeNumber
                   return $ valence:magnitude

decimalNumber :: P.Parser String
decimalNumber = do left <- integerNumber
                   middle <- P.option '.' $ P.char '.'
                   right <- P.option "" $ P.many P.digit
                   return $ left ++ ( middle : right )

scientificNumber :: P.Parser String
scientificNumber = do left <- decimalNumber
                      right <- P.option "e0" wholeNumber
                      return $ left ++ right

significand :: P.Parser String
significand = do e <- P.oneOf "eE"
                 n <- wholeNumber
                 return n

dateNumber :: P.Parser String
dateNumber = do left <- literal'
                right <- P.char 'd'
                return $ '\'':left ++ '\'':[right]

number :: P.Parser Word
number = do x <- dateNumber P.<|> scientificNumber
            return $ Number x

p t = P.parse t "(unspecified source)"
