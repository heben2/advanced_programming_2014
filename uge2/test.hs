{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

data Expr = Zero
          | One
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr2
            deriving (Show)

data Expr2 = Two | Three
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

t2Parser :: Parser Expr2
t2Parser = parse0 <|> parse1
  where parse0 = do symbol "2"
                    return Two
        parse1 = do symbol "3"
                    return Three

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x
{-
eParser :: Parser Expr
eParser = e1Parser <|> e2Parser

e1Parser :: Parser Expr
e1Parser = parse0
  where parse0 = do t <- e2Parser
                    m <- multop
                    t2 <- t2Parser
                    return (m t t2)
        multop = do symbol "*"
                    return Mul

e1Parser :: Parser Expr
e1Parser = parse0
  where parse0 = do t <- e2Parser
                    m <- multop
                    t2 <- t2Parser
                    return (m t t2)
        multop = do symbol "*"
                    return Mul

e2Parser :: Parser Expr
e2Parser = tParser `chainl1` (plusop <|> subop) 
    where plusop = do symbol "+"
                      return Add
          subop = do symbol "-"
                     return Sub
-}

eParser :: Parser Expr
eParser = chainl e1Parser multop e2Parser
      where multop = do symbol "*"
                        return Mul

e2Parser :: Parser Expr2
e2Parser = t2Parser

e1Parser :: Parser Expr
e1Parser = tParser `chainl1` (plusop <|> subop) 
    where plusop = do symbol "+"
                      return Add
          subop = do symbol "-"
                     return Sub

entry :: Parser Expr
entry = spaces *> eParser <* eof
