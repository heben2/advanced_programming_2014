import Text.ParserCombinators.ReadP
import Data.Char(isSpace)

{- Exercise 1 -}

{-

DIGIT ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
MANY-DIGIT ::= DIGIT MANY-DIGIT | e
MANY1-DIGIT ::= DIGIT MANY-DIGIT
MANY-INT ::= "," MANY1-INT | e
MANY1-INT ::= INT | MANY-INT
LIST ::= "[" MANY-INT "]"

-}

{- Exercise 2 -}

{-
         E1
E   ::= T E'
         E'1    E'2     E'3
E'  ::= "+" E | "-" E | e
        T1    T2
T   ::= "0" | "1"

First(T) = First(T1) U First(T2) = First("0") U First("1") = {"0", "1"}
First(E') = First(E'1) U First(E'2) U First(E'3) =
  = First("+"E) U First("-"E) U First(e) =
  = First("+") U First("-") U {} = {"+", "-"}
T is nullable
First(E) = First(E1) = First(T E') = First(T) U First(E') =
  = {"0", "1"} U { "+", "-"} = {"0", "1", "+", "-"}

Follow(E) \subseteq Follow(E') = {}
Follow(E') \subseteq Follow(E) = {}
Follow(T) \subseteq First(E') U Follow(E) = {"0", "1", "+", "-"}

Select(E1) = First(E1) = First(T E') = First(T) U First(E') =
  = {"0", "1"} U { "+", "-"} = {"0", "1", "+", "-"}
Select(E'1) = {"+"}
Select(E'2) = {"-"}
Select(E'3) = {}
Select(T1) = {"0"}
Select(T2) = {"1}

-}

{- Exercise 3 -}

{- Grammar requirements: page 15 -}

{- Exercise 18 -}

data SExp = IntVal Int
          | SymbolVal String
          | ListExp [SExp]
          deriving (Show, Eq, Ord)

parseProgram :: ReadP SExp
parseProgram = do
  sexp <- parseSExp
  eof -- IMPORTANT
  return sexp

parseSExp :: ReadP SExp
parseSExp = parseInt <++ parseList <++ parseSymbol {- biased choice -}

parseInt :: ReadP SExp
parseInt = do
  sign <- do { _ <- char '-'; return (-1) } +++ (return 1)
  digits <- many1 (satisfy (`elem` "0123456789"))
  let val = sign * (read digits)
  return $ IntVal val

parseSymbol :: ReadP SExp
parseSymbol = do
  val <- many1 (satisfy (not.isSpace))
  return $ SymbolVal val

parseList :: ReadP SExp
parseList = do
  charToken '('
  sexps <- many parseSExp
  charToken ')'
  return $ ListExp sexps

charToken :: Char -> ReadP ()
charToken c = do
  skipSpaces
  _ <- char c
  skipSpaces

stringToken :: String -> ReadP ()
stringToken s = do
  skipSpaces
  _ <- string s
  skipSpaces
