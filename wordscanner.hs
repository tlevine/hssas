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
             body <- P.many $ P.noneOf [quoteChar]
             _ <- P.char quoteChar
             return $ Literal body
  where
    escaped quoteChar = do x <- P.string [quoteChar, quoteChar]
                           return quoteChar
    bodyInit quoteChar = P.noneOf [quoteChar] P.<|> escaped quoteChar
    bodyLast quoteChar = do P.char quoteChar
                            P.notFollowedBy (P.char quoteChar)
                            return quoteChar

p t = P.parse t "(unspecified source)"
