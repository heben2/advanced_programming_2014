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
    [(round $ fromIntegral x1 + fromIntegral a * d1, round $ fromIntegral y1 + fromIntegral a * d2) | a <- [1..n]]
    where d1 = fromIntegral (x2-x1)/ fromIntegral n
          d2 = fromIntegral (y2-y1)/ fromIntegral n

--
-- Define the types Context and SalsaCommand
--
type Environment = ([Definition], [Ident], Int) --Defintions, active views, framerate
type State = M.Map Ident (Ident,Pos) --View (Shape, Pos)
data Context = Context Environment State
-----------------------------------------
--type Environment = M.Map String SExp
--emptyEnvironment :: Environment
--emptyEnvironment = M.empty
-----------------------------------------
type Error = String

newtype SalsaCommand a = SalsaCommand { runSC :: Context -> a }
--TODO
--Note that a command cannot change the environment, 
--your type should reflect this.
--HOW to do this in the type? This is done in the instance??

instance Monad SalsaCommand where
  return x = SalsaCommand $ const x
  m >>= f = SalsaCommand $ \con -> let x = runSC m con
                                   in runSC (f x) con


-- functions for manipulating the context



--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command = undefined

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