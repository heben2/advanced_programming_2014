{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

--Create the function symbol to handle white spaces. Always use this

{-data T = Zero | One
        deriving(Show)
data E = Add T E
       | Sub T E
       | Const T
       deriving(Show)

--tParser :: Parser T
tParser = parse0 <|> parse1
        where parse0 = do symbol "0"
                          return Zero
              parse1 = do symbol "1"
                          return One
-}

{-
    E ::= T "+" E | T "-" E | T

Is transformed to this:

    E ::= T E'
    E' ::= "+" E
         | "-" E
         | epsilon

    T ::= "0" | "1"
-}


symbol :: String -> Parser ()
symbol s = do string s
              spaces
              return ()

data Expr = Zero
          | One
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
            deriving (Show)




constParser :: Parser Expr
constParser = parse0 <|> parse1
        where parse0 = do symbol "0"
                          return Zero
              parse1 = do symbol "1"
                          return One

{-
eParser :: Parser E
eParser = do t <- tParser
            let eParser' = 
                parseP <|> parseM <|> Const t
-}
eParser :: Parser Expr
eParser = e1Parser `chainl1` (plusop <|> subop)
        where
            plusop = do string "+"
                        return Add
            subop = do  string "-"
                        return Sub
e1Parser :: Parser Expr
e1Parser = constParser `chainl1` mulop
        where
            mulop = do string "*"
                       return Mul

-- Always use this to force parser to go through whole symbol.
entry :: Parser Expr
entry = do e <- spaces
           e <- eParser
           eof
           return e
-- Now call parser with   parseTest entry "...."