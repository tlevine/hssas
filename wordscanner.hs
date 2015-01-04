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

-- literal :: P.Parser Word
literal = do quoteChar <- P.oneOf "\"'"
             body <- P.many $
                              normalString quoteChar  P.<|> 
                                escaped quoteChar
             P.char quoteChar
             return $ body
  where
    escaped quoteChar = do x <- P.string _ -- [quoteChar, quoteChar]
                           return quoteChar
    normalString quoteChar = do x <- P.many $ P.noneOf [quoteChar]
                                return $ x

p t = P.parse t "(unspecified source)"
