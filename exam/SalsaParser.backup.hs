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

def :: Parser Definition
def = viewdef <|> rectangle <|> circle <|> view <|> group
    where viewdef = do      symbol "viewdef"
                            i <- vIdent
                            e1 <- expr
                            e2 <- expr
                            return $ Viewdef i e1 e2
          rectangle = do    symbol "rectangle"
                            i <- sIdent
                            e1 <- expr
                            e2 <- expr
                            e3 <- expr
                            e4 <- expr
                            c <- color
                            return $ Rectangle i e1 e2 e3 e4 c
          circle = do       symbol "circle"
                            i <- sIdent
                            e1 <- expr
                            e2 <- expr
                            e3 <- expr
                            c <- color
                            return $ Circle i e1 e2 e3 c
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

com :: Parser Command
com = undefined

expr :: Parser Expr
expr = undefined

exprOp :: Parser Expr
exprOp = undefined


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
                 return x y


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




--TODO Remember to remove following spaces
number :: Parser Number
number = do digits <- many1 digit
            let val = read digits
            do string "."
               decimals <- many1 digit
               return $ read $ digits++"."++decimals
               <|> return val




-- Handles all following spaces
--symbol :: String -> Parser ()
--symbol s = do string s
--              spaces
--              return ()

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
keyword s = token $ string s >> notFollowedBy constituent