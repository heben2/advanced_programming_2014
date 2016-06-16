 module CurveInterp where

import Control.Monad
import qualified Data.Map as M

import CurvySyntax
import CurveAST as AST
import Curve as C

type Environment = M.Map Ident C.Curve

lookupCurve :: Ident -> Environment -> Maybe C.Curve
lookupCurve = M.lookup

insertCurve :: Ident -> C.Curve -> Environment -> Environment
insertCurve = M.insert

getCurves :: Environment -> [C.Curve]
getCurves = M.elems

newtype CurveInterp a = CI { runCI :: Environment -> Maybe a }

instance Monad CurveInterp where
  return x     = CI $ \_ -> Just x
  m >>= f = CI $ \env -> case runCI m env of
                           Nothing -> Nothing
                           Just x -> runCI (f x) env

local :: (Environment -> Environment) -> CurveInterp a -> CurveInterp a
local f m = CI $ \env -> let env' = f env
                         in runCI m env'
ask :: CurveInterp Environment
ask = CI $ \env -> Just env
invalid :: CurveInterp a
invalid = CI $ \_ -> Nothing
lookupCurveM :: Ident -> CurveInterp C.Curve
lookupCurveM name = do env <- ask
                       case lookupCurve name env of
                         Just c  -> return c
                         Nothing -> invalid

interpString :: String -> Either String [C.Curve]
interpString s = case parseString s of
                   Left e -> Left $ show e
                   Right prog ->
                     case interp prog of
                       Nothing -> Left "Interpretation failed"
                       Just cs -> Right cs

interp :: Program -> Maybe [C.Curve]
interp prog = do env <- runCI (go prog) emptyEnv
                 return $ getCurves env
  where
    emptyEnv :: Environment
    emptyEnv = M.empty

    evalCurveDef :: AST.Def -> CurveInterp (Ident, C.Curve)
    evalCurveDef (Def curveName curveDef subCurves) = do
      cs <- mapM evalCurveDef subCurves
      local (addToEnv cs) $ do
        c <- evalCurve curveDef
        return (curveName, c)

    addToEnv :: [(Ident, C.Curve)] -> Environment -> Environment
    addToEnv [] env = env
    addToEnv ((name, c):cs) env =
      addToEnv cs (insertCurve name c env)

    go :: Program -> CurveInterp Environment
    go [] = ask
    go (def : defs) = do
      (curveName, cCurve) <- evalCurveDef def
      local (insertCurve curveName cCurve) $
        go defs

evalCurve :: AST.Curve -> CurveInterp C.Curve
evalCurve (AST.Single (AST.Point ex ey)) = do
  dx <- evalExpr ex
  dy <- evalExpr ey
  return $ C.curve (C.point (dx, dy)) []
evalCurve (AST.Id curveName) =
  lookupCurveM curveName
evalCurve (AST.Connect c1 c2) =
  liftM2 C.connect (evalCurve c1) (evalCurve c2)
evalCurve (AST.Over _ _) =
  error "Let's just pretend"
evalCurve (AST.Translate c p) =
  liftM2 translate (evalCurve c) (evalPoint p)
evalCurve _ = error "Let's pretend some more"

evalExpr :: AST.Expr -> CurveInterp Double
evalExpr (Mult e1 e2) =
  liftM2 (*) (evalExpr e1) (evalExpr e2)
evalExpr (Add e1 e2) =
  liftM2 (+) (evalExpr e1) (evalExpr e2)
evalExpr (Width c) = liftM width $ evalCurve c
evalExpr (Height c) = liftM height $ evalCurve c
evalExpr (Const x) = return x

evalPoint :: AST.Point -> CurveInterp C.Point
evalPoint (AST.Point e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  return $ point (x, y)
