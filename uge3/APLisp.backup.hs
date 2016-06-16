{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module APLisp where

import Text.ParserCombinators.ReadP
import Data.Char(isSpace,isDigit)
import Control.Monad
import qualified Data.Map as M

data SExp = IntVal Int
          | SymbolVal String
          | ListExp [SExp]
            deriving (Show, Eq, Ord)

type Parser a = ReadP a

(<|>) = (+++)
parse = readP_to_S

token           :: Parser a -> Parser a
token p          = skipSpaces >> p

symbol           = token . string
schar            = token . char

numberOrSymbol :: Parser SExp
numberOrSymbol = token $ do s <- munch1 $ \c -> not(isSpace c || c `elem` "()")
                            return $ if all isDigit s then IntVal $ read s
                                     else SymbolVal s
sexp :: Parser SExp
sexp = numberOrSymbol
       <|> between (schar '(') (schar ')') sexps
  where sexps = Control.Monad.liftM ListExp (many sexp)
    --many sexp >>= return . ListExp

parseString :: String -> Either String SExp
parseString s =
  case parse (do {e <- sexp; token eof; return e}) s of
      [(e, [])] -> Right e
      _         -> Left "Parse error"

--------------------------------------------------------------------------------
{- The interpreter of APLisp, using the above parser -}
type Environment = M.Map String SExp
emptyEnvironment :: Environment
emptyEnvironment = M.empty
--Temporary error type for APLispExec
type Error = String
type Result = Either Error SExp
newtype APLispExec a = RC { runLisp :: Environment -> Either Error a}

instance Monad APLispExec where
  return x     = RC $ \_ -> Right x
  m >>= f = RC $ \env -> case runLisp m env of
                           Left e -> Left e
                           Right x -> runLisp (f x) env

--Returns False if the given value represents an empty list, True otherwise.
true :: SExp -> Bool
true (ListExp []) = False
true _ = True

--If the name is already bound in the environment,the new binding should replace the old.
bindVar :: String -> SExp -> Environment -> Environment
bindVar = M.insert

lookupVar :: String -> Environment -> Maybe SExp
lookupVar = M.lookup

--Return given error message as error.
invalid :: String -> APLispExec a
invalid s = RC $ \_ -> Left s

--Returns monad with the current environment
getCurrentEnv :: APLispExec Environment
getCurrentEnv = RC $ \env -> Right env

--Looks up variable in environment. If present, returned as monad. Else return error.
lookupVarM :: String -> APLispExec SExp
lookupVarM name = do env <- getCurrentEnv
                     case lookupVar name env of
                      Just s -> return s
                      Nothing -> invalid "No such variable defined"

{- Make local environment defined by f and run that on the given monad m.
   Return the result to the current monad RC. -}
local :: (Environment -> Environment) -> APLispExec a -> APLispExec a
local f m = RC $ \env -> let env' = f env
                         in runLisp m env'

eval :: SExp -> APLispExec SExp
eval (SymbolVal s) = lookupVarM s
eval (IntVal v) = return $ IntVal v
eval (ListExp []) = return $ ListExp []
eval (ListExp [SymbolVal "quote", x]) = return x
eval (ListExp (SymbolVal "quote":_)) = 
    invalid "Quote called with wrong number of arguments."
eval (ListExp [SymbolVal "let", ListExp vars, body]) = do
    vars' <- mapM evalLet vars
    local (addToEnv vars') $ eval body -- Make local environment based on the variables in the let and apply to the evaluation of body.
    where
        addToEnv :: [(String, SExp)] -> Environment -> Environment -- Add pairs of variables binding to evaluated values.
        addToEnv [] env = env
        addToEnv ((var, val):args) env =
                 addToEnv args (bindVar var val env)
        evalLet :: SExp -> APLispExec (String, SExp) --Make pairs of variables binding to evaluated values.
        evalLet (ListExp[SymbolVal var, val]) = do 
          val' <- eval val
          return (var, val')
        evalLet _ = invalid "Something went wrong in let-statement."
eval (ListExp (SymbolVal "let":_)) = invalid "Invalid let-statement."
{-
--ALternative  by Troels:
eval (ListExp [SymbolVal "let", ListExp [], body]) = eval body
eval (ListExp [SymbolVal "let", ListExp (ListExp [SymbolVal x,e]:bnds, body]) = do
  v <- eval e
  local (bindVar x v) $ do
    eval (ListExp [SymbolVal "let", ListExp bnds, body])
-}
eval e@(ListExp (SymbolVal "lambda":ListExp _:_)) = return e --Note: can only evaluate/apply on [SymbolVal "lambda",ListExp [_,_]]
eval (ListExp (SymbolVal "lambda":_)) = fail "Bad lambda"
--Add handling of funcall here, and check there are two arguments lists, i.e. a lambda/symbolVal(will be looked up as a lambda) and an argumentlist

eval (ListExp [SymbolVal "funcall", f, ListExp args]) = do 
    f' <- eval f
    apply f' args
eval (ListExp [SymbolVal "funcall", f, arg]) = do 
    f' <- eval f
    apply f' [arg]
eval (ListExp (arg:args)) = do 
    args' <- mapM eval args
    apply arg args'


apply :: SExp -> [SExp] -> APLispExec SExp
--apply (ListExp [SymbolVal "lambda", ListExp [ListExp [param], body]]) [arg] =  eval (ListExp [SymbolVal "let", ListExp [ListExp [param, arg]], body])
apply (ListExp [SymbolVal "lambda", ListExp params, body]) args =  --Also accepts arguments (multiple) of non-list-type.
    if length params == length args then  --params might be a list, so it is [ListExp (SymbolVal "list"):args]
        eval (ListExp [SymbolVal "let", ListExp $ zipWith applyArg params args, body])
    else invalid "Number of arguments not equal to number of parameters."
    where 
        applyArg param arg = ListExp [param, arg]
apply (IntVal _) _ = invalid "Cannot apply integers!"
apply (ListExp _) _ = invalid "Cannot apply lists!"
apply (SymbolVal "+") [IntVal x1,IntVal x2] = return $ IntVal $ (+) x1 x2
apply (SymbolVal "-") [IntVal x1,IntVal x2] = return $ IntVal $ (-) x1 x2
apply (SymbolVal "*") [IntVal x1,IntVal x2] = return $ IntVal $ (*) x1 x2
apply (SymbolVal "/") [IntVal x1,IntVal x2] = if x2 == 0 then invalid "Cannot divide by 0!"
                                              else return $ IntVal $ div x1 x2
apply (SymbolVal "=") [x1,x2] = return $ if x1 == x2 then SymbolVal "True" else ListExp []
apply (SymbolVal "!=") [x1,x2] = return $ if x1 /= x2 then SymbolVal "True" else ListExp []
apply (SymbolVal "<") [IntVal x1,IntVal x2] = return $ if x1 < x2 then SymbolVal "True" else ListExp []
apply (SymbolVal ">") [IntVal x1,IntVal x2] = return $ if x1 > x2 then SymbolVal "True" else ListExp []
apply (SymbolVal "<=") [IntVal x1,IntVal x2] = return $ if x1 <= x2 then SymbolVal "True" else ListExp []
apply (SymbolVal ">=") [IntVal x1,IntVal x2] = return $ if x1 >= x2 then  SymbolVal "True" else ListExp []
apply (SymbolVal "list") xs = return $ ListExp xs
apply (SymbolVal "car") [l@(ListExp xs)] = if true l then return $ ListExp [head xs] else invalid "List is empty."
apply (SymbolVal "cdr") [l@(ListExp xs)] = if true l then return $ ListExp [last xs] else invalid "List is empty."
apply (SymbolVal "cons") [x1, ListExp xs] = return $ ListExp $ x1:xs
apply (SymbolVal "if") [c, texp, fexp] = return $ if true c then texp else fexp
apply (SymbolVal x) _ = invalid $ "Invalid symbol: "++x
-- I use partial functions head and last, because I guarantee that the list is non-empty.


interpret :: String -> Result
interpret s = do s' <- parseString s
                 runLisp (eval s') emptyEnvironment