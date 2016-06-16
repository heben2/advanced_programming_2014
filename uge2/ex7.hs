{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

data Expr = Zero
          | One
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
            deriving (Show)

symbol :: String -> Parser ()
symbol s = do string s
              spaces
              return ()

tParser :: Parser Expr
tParser = parse0 <|> parse1 <|> parsePs
  where parse0 = do symbol "0"
                    return Zero
        parse1 = do symbol "1"
                    return One
        parsePs = parens eParser

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

eParser :: Parser Expr
eParser = e1Parser `chainl1` (plusop <|> subop)
  where plusop = do symbol "+"
                    return Add
        subop = do symbol "-"
                   return Sub

e1Parser :: Parser Expr
e1Parser = tParser `chainl1` multop
  where multop = do symbol "*"
                    return Mul

entry :: Parser Expr
entry = spaces *> eParser <* eof
