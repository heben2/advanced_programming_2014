{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CurvySyntax 
( parseString
, parseFile
) where

import CurveAST
--import Data.Char(isSpace)
import Text.Parsec
--import Text.Parsec.Combinator
import Text.Parsec.String

-- Use imported errors of Parsec.
type Error = ParseError

parseString :: String -> Either Error Program
parseString = parse program ""

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

program :: Parser Program
program = do _ <- spaces
             --_ <- skipMany newline
             --_ <- skipMany tab
             e <- defs
             eof
             return e

defs :: Parser Program
defs = many1 def

def :: Parser Def
def = do i <- ident
         symbol "="
         c <- curve
         do {symbol "where"; symbol "{"; d <- defs; symbol "}"; return (Def i c d)} 
             <|> return (Def i c [])

curve :: Parser Curve
curve = curveOp

curveOp :: Parser Curve
curveOp = curveOp1 `chainl1` connectOp
      where connectOp   = do symbol "++"
                             return Connect

curveOp1 :: Parser Curve
curveOp1 = curveOp2 `chainl1` overOp
      where overOp      = do symbol "^"
                             return Over

curveOp2 :: Parser Curve
curveOp2 = (curveOp3, point) `chainl2` translateOp
      where translateOp = do symbol "->"
                             return Translate

curveOp3 :: Parser Curve
curveOp3 = (curveOp4, expr) `chainl2` scaleOp
      where scaleOp     = do symbol "**"
                             return Scale

curveOp4 :: Parser Curve
curveOp4 = (curveConst, expr) `chainl2` (try refvOp <|> try refhOp <|> try rotOp)
      where refvOp      = do symbol "refv"
                             return Refv
            refhOp      = do symbol "refh"
                             return Refh
            rotOp       = do symbol "rot"
                             return Rot

curveConst :: Parser Curve
curveConst = parse0 <|> parse1 <|> parse2
        where parse0 = do i <- ident
                          return (Id i)
              parse1 = do p <- try point
                          return (Single p)
              parse2 = parens curve

point :: Parser Point
point = do symbol "("
           e1 <- expr
           symbol ","
           e2 <- expr
           symbol ")"
           return (Point e1 e2)

expr :: Parser Expr
expr = exprOp `chainl1` plusOp
        where plusOp = do symbol "+"
                          return Add

exprOp :: Parser Expr
exprOp = exprConst `chainl1` mulop
        where mulop = do symbol "*"
                         return Mult

exprConst :: Parser Expr
exprConst = parse0 <|> parse1 <|> parse2
        where parse0 = do symbol "width"
                          c <- curve
                          return (Width c)
              parse1 = do symbol "height"
                          c <- curve
                          return (Height c)
              parse2 = do n <- number
                          _ <- symbol "" --To handle spaces after the number n.
                          return (Const n)


{-Where Ident is a a non-empty sequence of letters, digits and underscores (_), 
that is not one of the reserved words: where, refv, refh, rot, width, or height.-}
-- alphaNum takes all but (_)...
ident :: Parser Ident
ident = do s <- many1 (satisfy (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxyz0123456789_"))
           _ <- symbol "" --To handle spaces after the ident s.
           if s `notElem` ["where", "refv", "refh", "rot", "width", "height"] then return s else fail $ "'"++s++"' is a reserved word."

number :: Parser Number
number = do digits <- many1 digit
            let val = read digits
            do string "."
               decimals <- many1 digit
               return $ read $ digits++"."++decimals
               <|> return val

symbol :: String -> Parser ()
symbol s = do string s
              spaces
--             skipMany newline
--             skipMany tab
              return ()

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x


chainl2 :: (Stream s m t) => (ParsecT s u m a, ParsecT s u m b) -> ParsecT s u m (a -> b -> a) -> ParsecT s u m a
chainl2 (p,q) op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- q
                                    ; rest (f x y)
                                    }
                                <|> return x