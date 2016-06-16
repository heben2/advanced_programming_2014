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
data Context = Context Environment State

emptyState :: State
emptyState = M.empty

newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (State, a) }

instance Monad SalsaCommand where
  return x = SalsaCommand $ \(Context _ state) -> (state,x)
  m >>= f = SalsaCommand $ \con@(Context env _) -> 
                                let (state',x) = runSC m con
                                in runSC (f x) (Context env state')


-- functions for manipulating the context

--TODO: Helper function for creating State from Environmenet?

--Returns monad with the current environment TODO: USED??
getCurrentCon :: SalsaCommand Context
getCurrentCon = SalsaCommand $ \con@(Context _ state) -> (state, con)


--
-- Define the function command
--
{-
SalsaCommand $ \ M.mapWithKey moveWithKey
                              return ()
-}

command :: Command -> SalsaCommand ()
command (Move shapeIds pos) = 
    SalsaCommand $ \con@(Context env state) -> let state' = (M.mapWithKey moveWithKey state)
                                                                       in runSC () (Context env state')
    where moveWithKey key x = if key `elem` shapeIds then
                                case pos of
                                    Abs e1 e2 -> map (\(i,(x,y)) -> (e1,e2)) x
                                    Rel e1 e2 -> map (\(i,(x,y)) -> (x+e1,y+e2))  x
                              else x

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