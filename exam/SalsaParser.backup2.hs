--
-- Skeleton for Salsa parser
-- To be used at the exam for Advanced Programming, B1-2013
-- Edited by Henrik Bendt, gwk553, November 2013
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SalsaParser 
( parseString
, parseFile
) where

import SalsaAst
import Text.Parsec

type Error = ParseError --Use errors from Parsec

parseString :: String -> Either Error Program
parseString = parse program ""

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

program :: Parser Program
program = spaces *> many1 defCom <* eof
--    do 
--    _ <- spaces
--    e <- many1 defCom
--    eof
--    return e


defCom :: Parser DefCom
defCom = try d <|> c
        where d = do d' <- def
                     return $ Def d'
              c = do c' <- com
                     return $ Com c'

--TODO test might do wrong. This is hlint that is complaining
def :: Parser Definition
def = viewdef <|> rectangle <|> circle <|> view <|> group
    where viewdef = do      symbol "viewdef"
                            i <- vIdent
                            e1 <- expr
                            e2 <- expr
                            return $ Viewdef i e1 e2
          rectangle = do    symbol "rectangle"
                            a <- getExpr
                            e4 <- test
                            c <- color
                            return $ Rectangle a e4 c
          circle = do       symbol "circle"
                            a <- test
                            c <- color
                            return $ Circle a c
          view = do         symbol "view"
                            i <- vIdent
                            return $ View i
          group = do        symbol "group"
                            i <- vIdent
                            symbol "["
                            is <- many1 vIdent
                            symbol "]"
                            return $ Group i [is]
          color = ret (symbol "blue") Blue
              <|> ret (symbol "plum") Plum
              <|> ret (symbol "red") Red
              <|> ret (symbol "green") Green
              <|> ret (symbol "orange") Orange
          ret s f = s >> return f
          test = do         i <- sIdent
                            e1 <- expr
                            e2 <- expr
                            e3 <- expr
                            return i e1 e2 e3

com :: Parser Command
com = c0
    where c0 = c1 `chainl1` op (symbol "||") Par
          c1 = (c2, vident) `chainl2` op (symbol "@") At --(do {symbol "@"; return At})
          c2 = <|> ""
          op s f = s >> return f



Command ::= SIdents '->' Pos CommandOp
          | '{' Command '}'
CommandOp ::= '@' VIdent
            | '||' Command
-----
curve :: Parser Curve
curve = c0
  where c0 = c1 `chainl1` op (symbol "++") Connect
        c1 = c2 `chainl1` op (symbol "^") Over
        c2 = bop Translate c3 (symbol "->") point
        c3 = bop Scale c4 (symbol "**") expr
        c4 = bop Refv c5 (keyword "retv") expr
        c5 = bop Refh c6 (keyword "reth") expr
        c6 = bop Rot primcurve (keyword "rot") expr
        op s f = s >> return f

bop :: (a -> b -> a) -> Parser a -> Parser () -> Parser b -> Parser a
bop f l op r = do x <- l
                  bop' x
  where bop' x = try (do op
                         y <- r
                         bop' $ f x y)
                 <|> return x

---------




expr :: Parser Expr
expr = prim `chainl1` (plusOp <|> minusOp)
    where plusop = do   symbol "+"
                        return Plus
          subop = do    symbol "-"
                        return Minus
          prim = par <|> int <|> proj
              where par = do    symbol "("
                                x <- expr
                                symbol ")"
                                return x
                    int = do    digits <- many1 digit
                                let i = read digits
                                spaces
                                return $ Const i
                    proj = do   i <- sIdent
                                symbol "."
                                do symbol "x"
                                   return $ Xproj i
                                   <|> do symbol "y"
                                          return $ Yproj i

pos :: Parser Pos
pos = p1 <|> p2
    where p1 = do symbol "+"
                  ps <- p
                  return $ Rel ps
          p2 = do ps <- p
                  return $ Abs ps
          p = do symbol "("
                 x <- expr
                 symbol ","
                 y <- expr
                 symbol ")"
                 return x y --Try adding $ in front in case of error

vIdent :: Parser Ident
vIdent = token $ do
    l1 <- upper
    ls <- many1 constituent
    return $ l1++ls

sIdent :: Parser Ident
sIdent = token $ do
    l1 <- lower
    ls <- many1 constituent
    let l = l1++ls
    if l `elem` reserved then
        fail $ "'" ++ l ++ "' is a reserved word."
    else return l



parens :: String -> String -> Parser a -> Parser a
parens s1 s2 p = do symbol s1
              x <- p
              symbol s2
              return x

-- Helper function working like chail1, though expanded to take another parser type
chainl2 :: (Stream s m t) => (ParsecT s u m a, ParsecT s u m b) -> ParsecT s u m (a -> b -> a) -> ParsecT s u m a
chainl2 (p,q) op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- q
                                    ; rest (f x y)
                                    }
                                <|> return x


-- Helper functions from Reference Solutions of Absalon
token :: Parser a -> Parser a
token p = do x <- p
             spaces
             return x

symbol :: String -> Parser ()
symbol = void . token . string

reserved :: [String]
reserved = ["viewdef", "rectangle", "circle", "group", "view", "blue", "plum", "red", "green", "orange"]

constituent :: Parser Char
constituent = alphaNum <|> char '_'

--TOOD Used??
keyword :: String -> Parser ()
keyword s = token $ string s >> notFollowedBy constituentS