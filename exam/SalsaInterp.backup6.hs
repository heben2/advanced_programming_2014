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
type State = M.Map Ident [(Ident,Position)] --Shape [( (active) View, (current) Position)]
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

-- Calculates given expression base on given state
-- All expressions computes to integers
calcExpr :: Expr -> State -> Integer
calcExpr (Const i) _        = i
calcExpr (Plus e1 e2) s     = (+) (calcExpr e1 s) (calcExpr e2 s)
calcExpr (Minus e1 e2) s    = (-) (calcExpr e1 s) (calcExpr e2 s)
calcExpr (Xproj i) s        = 
    case M.lookup i s of
        Just a@((_,(k,_)):_) -> 
            foldr (\(_,(x,_)) currInt -> min currInt x) 
            k a
        _ -> error "X-indent not found"
--        Nothing -> error "X-indent not found"
calcExpr (Yproj i) s        = 
    case M.lookup i s of
        Just a@((_,(_,k)):_) -> 
            foldr (\(_,(_,y)) currInt -> min currInt y) 
            k a
        _ -> error "Y-indent not found" -- All shapes should have positions.
--        Nothing -> error "Y-indent not found" 

--TODO? USED???
getState :: SalsaCommand State
getState = SalsaCommand $ \s -> (s,s)

--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command (Move shapeIds pos) = 
    SalsaCommand $ \state -> 
    ((), M.mapWithKey (\key a -> 
            if key `elem` shapeIds then
                case pos of
                    Abs e1 e2 -> map (\(i,_) -> 
                                      (i,(calcExpr e1 state, calcExpr e2 state))) a
                    Rel e1 e2 -> map (\(i,(x,y)) -> 
                                      (i,(x + calcExpr e1 state,y + calcExpr e2 state))) a
            else a)
            state)
command (At com i) = -- command com only affects view(s) i (can point to group)
    SalsaCommand $ \state -> ((),
                              let (_, state') = runSC (command com) state
                              in 
                             )
command (Par c1 c2) = --execute c1 and c2 concurrently. May not manipulate same shape.
    SalsaCommand $ \state -> ((),
                              let (_, state1) = runSC (command c1) state
                                  (_, state2) = runSC (command c2) state
                                  (_, diff1) = M.partitionWithKey 
                                                (\k a -> case M.lookup k state of
                                                            Just x -> a == x
                                                            Nothing -> True
                                                ) state1
                                  (_, diff2) = M.partitionWithKey 
                                                (\k a -> case M.lookup k state of
                                                            Just x -> a == x
                                                            Nothing -> True
                                                ) state2
                              in if M.difference diff1 diff2 == emptyState then
                                    M.unions [diff1, diff2, state]
                                    --(diff1 `M.union` state) `M.union` diff2
                                 else error "Concurrent commands manipulates same shapes"
                             )


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