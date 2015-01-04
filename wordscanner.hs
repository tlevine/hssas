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
literal = do quoteChar <- P.oneOf "\"'"
             body <- P.many $ character quoteChar
             _ <- P.char quoteChar
             return $ Literal body
  where
    character quoteChar = (P.noneOf [quoteChar]) P.<|>
                          (P.try (P.string [quoteChar, quoteChar] >> return quoteChar))

integer :: P.Parser String
integer = do valence <- P.option '+' $ P.char '-'
             magnitude <- P.many P.digit
             return $ valence:magnitude

decimal :: P.Parser String
decimal = do left <- integer
             middle <- P.char '.'
             right <- P.many P.digit
             return $ left ++ ( middle : right )

p t = P.parse t "(unspecified source)"
