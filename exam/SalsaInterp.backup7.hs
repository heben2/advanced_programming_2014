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
import qualified Data.Maybe as MB

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
--type Environment = ([Definition], [Ident], Integer) --Defintions, active views, framerate

data Environment = Environment 
        {
            views  :: M.Map ViewName (Integer, Integer),
            groups :: M.Map String [String],
            shapes :: M.Map String (Integer, Integer, Integer, Integer, ColourName),
            aViews :: [ViewName],
            framerate :: Integer
        }


type State = M.Map String [(ViewName, Position)] --Shape [( View, (current) Position)]
data Context = Context Environment State

emptyState :: State
emptyState = M.empty

newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (a, State) }

instance Monad SalsaCommand where
    return x = SalsaCommand $ \(Context _ state) -> (x, state)
    (SalsaCommand m) >>= f = SalsaCommand $ \(Context env state) -> 
                                let (x, state') = m (Context env state)
                                    (SalsaCommand g) = f x
                                in g (Context env state')


-- functions for manipulating the context

-- Calculates given expression base on given state
-- All expressions computes to integers
calcExpr :: Expr -> Context -> Integer
calcExpr (Const i) _                = i
calcExpr (Plus e1 e2) c             = (+) (calcExpr e1 c) (calcExpr e2 c)
calcExpr (Minus e1 e2) c            = (-) (calcExpr e1 c) (calcExpr e2 c)
calcExpr (Xproj i) (Context env s)  = 
    case M.lookup i s of
        Just a@((_,(k,_)):_) -> 
            foldr (\(vId,(x,_)) currInt -> if vId `elem` aViews env then min currInt x else currInt) 
            k a
        _ -> error "X-indent not found"
--        Nothing -> error "X-indent not found"
calcExpr (Yproj i) (Context env s)  = 
    case M.lookup i s of
        Just a@((_,(_,k)):_) -> 
            foldr (\(vId,(_,y)) currInt -> if vId `elem` aViews env then min currInt y else currInt) 
            k a
        _ -> error "Y-indent not found" -- All shapes should have positions.
--        Nothing -> error "Y-indent not found" 

getColor :: Colour -> ColourName
getColor c = case c of
                Blue    -> "blue"
                Plum    -> "plum"
                Red     -> "red"
                Green   -> "green"
                Orange  -> "orange"

-- Returns given dimension if positive
checkDim :: Integer -> Integer
checkDim d = if d < 0 then error "Cannot have negative dimensions" else d


-- Returns list of idents.
-- Ident can be a group. No check if group idents are of views.
getViewId :: Environment -> ViewName -> [String]
getViewId env ident = 
    case M.lookup ident $ views env of
        Just _ -> [ident]
        Nothing -> getGroupMems env ident

-- Get idents of group.
getGroupMems :: Environment -> String -> [String]
getGroupMems env gId = 
    MB.fromMaybe (error "Group id not found.") 
      (M.lookup gId $ groups env)


--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command (Move shapeIds pos) = 
    SalsaCommand $ \con@(Context env state) -> 
        ((), 
         M.mapWithKey (\k a -> 
            if k `elem` shapeIds && k `elem` aViews env then
                case pos of
                    Abs e1 e2 -> map (\(i,_) -> 
                                      (i,(calcExpr e1 con, calcExpr e2 con))) a
                    Rel e1 e2 -> map (\(i,(x,y)) -> 
                                      (i,(x + calcExpr e1 con, y + calcExpr e2 con))) a
            else a)
            state
        )
command (At com i) = -- command com only affects view(s) i (can point to group)
    SalsaCommand $ \(Context env state) -> 
        ((),
         let members = getViewId env i
             env' = Environment (views env) (groups env) (shapes env) 
                                members (framerate env)
             (_, state') = runSC (command com) (Context env' state)
         in state'
        )
command (Par c1 c2) = --execute c1 and c2 concurrently. May not manipulate same shape.
    SalsaCommand $ \con@(Context _ state) -> 
        ((),
         let (_, state1) = runSC (command c1) con
             (_, state2) = runSC (command c2) con
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

MB.fromMaybe (error "Group id not found.") 
      (M.lookup gId $ groups env)


--
-- Define the type Salsa
--

data Salsa a = Salsa (Context -> (a, Context))

instance Monad Salsa where
    return k              =  Salsa $ \con -> (k, con)
    Salsa m >>= f         =  Salsa $ \con -> let (x, con') = m con
                                                 Salsa g = f x
                                             in g con'

--
-- Define the functions liftC, definition, and defCom
--

-- Lift command to salsa, that is run the command and use new state.
liftC :: SalsaCommand a -> Salsa a
liftC sc = Salsa $ \con@(Context env _) -> 
                    let (a,state') = runSC sc con
                    in (a, Context env state')

-- Compute definition and update Salsa monad (that is, the context)
definition :: Definition -> Salsa ()
definition (Viewdef i e1 e2) = Salsa $ 
    \con@(Context env state) -> 
        ((),
         Context (Environment ((i, e1', e2'):views env) 
                  (groups env) (shapes env) 
                  [i] --TODO active view to i
                  (framerate env)) 
         state)
    where e1' = checkDim $ calcExpr e1 con
          e2' = checkDim $ calcExpr e2 con
definition (Rectangle i e1 e2 e3 e4 c) = Salsa $ 
    \con@(Context env state) -> 
        ((),
         let e1' = calcExpr e1 con --Hlint complained when it was in where..
             e2' = calcExpr e2 con
         in
         Context (Environment (views env) (groups env) 
                  ((i, e1', e2', e3', e4', getColor c):shapes env) 
                  (aViews env) (framerate env)) 
         state)
    where 
          e3' = checkDim $ calcExpr e3 con
          e4' = checkDim $ calcExpr e4 con
definition (Circle i e1 e2 e3 c) = Salsa $ 
    \con@(Context env state) -> 
        ((),
         Context (Environment (views env) (groups env) 
                  ((i, e1', e2', e3', getColor c):shapes env) 
                  (aViews env) (framerate env)) 
         state)
    where e1' = calcExpr e1 con
          e2' = calcExpr e2 con
          e3' = checkDim $ calcExpr e3 con
definition (View i) = Salsa $ 
    \con@(Context env state) -> 
        ((),
         Context (Environment (views env) (groups env) (shapes env) 
                  (getViewId i) 
                  (framerate env))
         state)

{-
views  :: M.Map ViewName (Integer, Integer),
groups :: M.Map String [String],
shapes :: M.Map String (Integer, Integer, Integer, Integer, ColourName),
aViews :: [ViewName],
framerate :: Integer
-}


defCom :: DefCom -> Salsa ()
defCom = undefined

--
-- Define the function runProg
--

runProg :: Integer -> Program -> Animation
runProg = undefined