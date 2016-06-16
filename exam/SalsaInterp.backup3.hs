--
-- Salsa interpreter
-- Edited by Henrik Bendt, gwk553, November 2013
--
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import Control.Monad
import qualified Data.Map as M 

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate n (x1,y1) (x2,y2) = 
    [(round $ x1' + fromIntegral a * d1, round $ y1' + fromIntegral a * d2) | a <- [1..n]]
    where d1 = (x2'-x1')/ n'
          d2 = (y2'-y1')/ n'
          x1' = fromIntegral x1
          x2' = fromIntegral x2
          y1' = fromIntegral y1
          y2' = fromIntegral y2
          n' = fromIntegral n

--
-- Define the types Context and SalsaCommand
--
type Environment = ([Definition], [Ident], Int) --Defintions, active views, framerate
type State = M.Map Ident [(Ident,Position)] --Shape [(View, Pos)]
--type MState s a = s -> (a,s)
data Context = Context Environment State

emptyState :: State
emptyState = M.empty

newtype SalsaCommand a = SalsaCommand { runSC :: State -> (a, State) } --Changed as Context contains environment, which is not used by commands

instance Monad SalsaCommand where
  return x = SalsaCommand $ \state -> (x, state)
  (SalsaCommand m) >>= f = SalsaCommand $ \state -> 
                                let (x, state') = m state
                                    (SalsaCommand g) = f x
                                in g state'


-- functions for manipulating the context

-- Calculates expressions
-- All expressions computes to integers
calcExpr :: Expr -> Integer
calcExpr (Const i)      = i
calcExpr (Plus e1 e2)   = (+) (calcExpr e1) (calcExpr e2)
calcExpr (Minus e1 e2)  = (-) (calcExpr e1) (calcExpr e2)
calcExpr (Xproj i)      = undefined

--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command (Move shapeIds pos) = 
    SalsaCommand $ \state -> ((), M.mapWithKey moveShape state)
    where moveShape key a = 
            if key `elem` shapeIds then
                case pos of
                    Abs e1 e2 -> map (\(i,_) -> 
                                      (i,(calcExpr e1, calcExpr e2))) a
                    Rel e1 e2 -> map (\(i,(x,y)) -> 
                                      (i,(x + calcExpr e1,y + calcExpr e2))) a
            else a
{-
command :: Command -> SalsaCommand ()
command (Move shapeIds pos) = 
    SalsaCommand $ \con@(Context env state) -> let state' = (M.mapWithKey moveWithKey state)
                                               in runSC () (Context env state')
    where moveWithKey key x = if key `elem` shapeIds then
                                case pos of
                                    Abs e1 e2 -> map (\(i,(x,y)) -> (e1,e2)) x
                                    Rel e1 e2 -> map (\(i,(x,y)) -> (x+e1,y+e2))  x
                              else x
-}
--
-- Define the type Salsa
--

--data Salsa a = undefined


--
-- Define the functions liftC, definition, and defCom
--

--liftC :: SalsaCommand a -> Salsa a
--liftC = undefined

--definition :: Definition -> Salsa ()
--definition = undefined

--defCom :: DefCom -> Salsa ()
--defCom = undefined

--
-- Define the function runProg
--

--runProg :: Integer -> Program -> Animation
--runProg = undefined