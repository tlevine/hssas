module Wordscanner where

data Word = Name String |
            Literal String |
            Number String |
            SpecialCharacter String
