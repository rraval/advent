module Parser where

import Control.Applicative
import Text.Trifecta

import Data

lexeme :: String -> Parser String
lexeme s = sp *> string s <* sp
  where sp = (skipMany $ oneOf " \t") <?> "white space"

instructionListParser = manyTill instructionParser (try $ spaces *> eof)

instructionParser = spaces *> go
  where go = Store <$> exprParser <* lexeme "->" <*> (WireName <$> some lower <?> "wire")

exprParser = choice
    [ binary And "AND"
    , binary Or "OR"
    , binary LShift "LSHIFT"
    , binary RShift "RSHIFT"
    , Not <$> (lexeme "NOT" *> operandParser)
    , Atom <$> operandParser
    ] <?> "expression"
  where binary ctr str = try $ ctr <$> operandParser <* lexeme str <*> operandParser

operandParser =
    (Literal . LiteralValue . read <$> some digit <?> "literal")
    <|> (Wire . WireName <$> some lower <?> "wire")
