module Macroscanner where

import qualified Text.ParserCombinators.Parsec as P
import           WordscannerHelpers           (name')

data MacroWord = MacroCall String
               | MacroSpecialCharacter String
               | MacroVariable String
               | NonMacro String

macroCall :: P.Parser MacroWord
macroCall = do percent <- P.char '%'
               macroName <- name'
               return $ MacroCall macroName

-- macroSpecialCharacter =
