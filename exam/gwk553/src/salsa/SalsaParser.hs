--
-- Salsa parser
-- Edited by Henrik Bendt, gwk553, November 2013
--
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SalsaParser 
( parseString
, parseFile
--, runTests
) where

import SalsaAst
import Text.Parsec.Prim hiding (token)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Error
import Control.Monad (void)

type Error = ParseError --Use errors from Parsec


-- Helper functions from CurvySyntax.hs from Reference Solutions on Absalon. 
token :: Parser a -> Parser a
token p = do x <- p
             spaces
             return x

symbol :: String -> Parser ()
symbol = void . token . string

reserved :: [String]
reserved = ["viewdef", "rectangle", "circle", "group", "view", "blue", "plum", 
            "red", "green", "orange"]

constituent :: Parser Char
constituent = alphaNum <|> char '_'

-- Handles multiple types of parentheses, defined by the two given strings.
parens :: String -> Parser a -> String -> Parser a
parens s1 p s2 = do symbol s1
                    x <- p
                    symbol s2
                    return x

-- Helper function working like chail1, though expanded to take another parser 
-- type to apply on right side after left recursion.
chainl2 :: (Stream s m t) => (ParsecT s u m a, ParsecT s u m b) -> 
            ParsecT s u m (a -> b -> a) -> ParsecT s u m a
chainl2 (p,q) op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- q
                                    ; rest (f x y)
                                    }
                                <|> return x

vIdent :: Parser Ident
vIdent = token $ do
    l1 <- upper
    ls <- many constituent
    return $ l1:ls

sIdent :: Parser Ident
sIdent = token $ do
    l1 <- lower
    ls <- many constituent
    let l = l1:ls
    if l `elem` reserved then
        fail $ "'" ++ l ++ "' is a reserved word."
    else return l

pos :: Parser Pos
pos = p1 <|> p2
    where p1 = do   symbol "+"
                    (e1, e2) <- p
                    return $ Rel e1 e2
          p2 = do   (e1, e2) <- p
                    return $ Abs e1 e2
          p  = do   symbol "("
                    x <- expr
                    symbol ","
                    y <- expr
                    symbol ")"
                    return (x,y)

expr :: Parser Expr
expr = prim `chainl1` (plusOp <|> minusOp)
    where plusOp = do   symbol "+"
                        return Plus
          minusOp = do  symbol "-"
                        return Minus
          prim = par <|> int <|> proj
              where par = parens "(" expr ")"
                    int = do    digits <- many1 digit
                                spaces
                                return $ Const $ read digits
                    proj = do   i <- sIdent
                                symbol "."
                                do symbol "x"
                                   return $ Xproj i
                                   <|> do symbol "y"
                                          return $ Yproj i

def :: Parser Definition
def = try viewdef <|> try rectangle <|> try circle <|> try view <|> group
    where viewdef = do      symbol "viewdef"
                            i <- vIdent
                            e1 <- expr
                            e2 <- expr
                            return $ Viewdef i e1 e2
          rectangle = do    symbol "rectangle"
                            (i,e1,e2,e3) <- figure
                            e4 <- expr
                            c <- color
                            return $ Rectangle i e1 e2 e3 e4 c
          circle = do       symbol "circle"
                            (i,e1,e2,e3) <- figure
                            c <- color
                            return $ Circle i e1 e2 e3 c
          view = do         symbol "view"
                            i <- vIdent
                            return $ View i
          group = do        symbol "group"
                            i <- vIdent
                            is <- parens "[" (many1 vIdent) "]"
                            return $ Group i is
          color = ret (symbol "blue") Blue
              <|> ret (symbol "plum") Plum
              <|> ret (symbol "red") Red
              <|> ret (symbol "green") Green
              <|> ret (symbol "orange") Orange
          ret s f = s >> return f
          figure = do       i <- sIdent
                            e1 <- expr
                            e2 <- expr
                            e3 <- expr
                            return (i, e1, e2, e3)

com :: Parser Command
com = c0
    where c0 = c1 `chainl1` op (symbol "||") Par
          c1 = (c2, vIdent) `chainl2` op (symbol "@") At 
          c2 = c3 <|> c4 
          c3 = parens "{" com "}"
          c4 = do is <- many1 sIdent
                  symbol "->"
                  p <- pos
                  return $ Move is p
          op s f = s >> return f

defCom :: Parser DefCom
defCom = try d <|> c
        where d = do d' <- def
                     return $ Def d'
              c = do c' <- com
                     return $ Com c'

program :: Parser Program
program = do _ <- spaces
             e <- many1 defCom
             eof
             return e

parseString :: String -> Either Error Program
parseString = parse program ""

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
