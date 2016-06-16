--
-- Salsa interpreter
-- Edited by Henrik Bendt, gwk553, November 2013
--
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SalsaInterp
    (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import qualified Data.Map as M 
import qualified Data.Maybe as MB
import qualified Data.List as L

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate n (x1,y1) (x2,y2) = 
    [(round $ x1' + a * d1, round $ y1' + a * d2) | a <- [1.0.. n']]
    where d1 = (x2'-x1')/ n'
          d2 = (y2'-y1')/ n'
          x1' = convertInt x1
          x2' = convertInt x2
          y1' = convertInt y1
          y2' = convertInt y2
          n' = if n < 0 then error "Frame rate cannot be negative." else convertInt n
          convertInt :: Integer -> Double
          convertInt = fromIntegral

--
-- Define the types Context and SalsaCommand
--

data Shape = NewRectangle Integer Integer Integer Integer ColourName
           | NewCircle Integer Integer Integer ColourName
data Environment = Environment 
        {
            views  :: M.Map ViewName (Integer, Integer), -- view name -> view dimensions
            groups :: M.Map String [String], --group id -> ids in group
            shapes :: M.Map String Shape, --shape id -> shape
            aViews :: [ViewName], --active view names
            framerate :: Integer
        }
type State = M.Map String [(ViewName, Position)] --Shape [( View, (current) Position)]
data Context = Context Environment State

emptyState :: State
emptyState = M.empty
emptyEnvironment :: Integer -> Environment
emptyEnvironment = Environment M.empty M.empty M.empty []
emptyContext :: Integer -> Context
emptyContext f = Context (emptyEnvironment f) emptyState
emptyFrames :: [Frame]
emptyFrames = []

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
            foldr (\(vId,(x,_)) currInt -> if vId `elem` aViews env 
                                             then min currInt x 
                                           else currInt) 
            k a
        _ -> error "X-indent not found"
--        Nothing -> error "X-indent not found"
calcExpr (Yproj i) (Context env s)  = 
    case M.lookup i s of
        Just a@((_,(_,k)):_) -> 
            foldr (\(vId,(_,y)) currInt -> if vId `elem` aViews env
                                             then min currInt y 
                                           else currInt) 
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
getViewId :: Environment -> ViewName -> [ViewName]
getViewId env ident = 
    if M.member ident $ views env then
        [ident]
    else getGroupMems env ident

-- Get idents of group.
getGroupMems :: Environment -> String -> [ViewName]
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
            if k `elem` shapeIds then
                case pos of
                    Abs e1 e2 -> 
                        map (\p@(i,_) -> 
                                if i `elem` aViews env then --Must be active view
                                    (i,(calcExpr e1 con, calcExpr e2 con))
                                else p) a
                    Rel e1 e2 -> 
                        map (\p@(i,(x,y)) -> 
                            if i `elem` aViews env then --Must be active view
                                (i,(x + calcExpr e1 con, y + calcExpr e2 con))
                            else p) a
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
             (_, diff1) = M.partitionWithKey  --Find changed shapes
                           (\k a -> case M.lookup k state of
                                       Just x -> a == x
                                       Nothing -> True
                           ) state1
             (_, diff2) = M.partitionWithKey 
                           (\k a -> case M.lookup k state of
                                       Just x -> a == x
                                       Nothing -> True
                           ) state2
         in if M.intersection diff1 diff2 == emptyState then -- must be no equal shapes
               M.unions [diff1, diff2, state]
            else error "Concurrent commands manipulates same shapes"
        )

--
-- Define the type Salsa
--

newtype Salsa a = Salsa {runS :: Context -> [Frame] -> (a, Context, [Frame])}

instance Monad Salsa where
    return k              =  Salsa $ \con f -> (k, con, f)
    Salsa m >>= f         =  Salsa $ \con frames -> 
                                    let (x, con', frames') = m con frames
                                        Salsa g = f x
                                    in g con' frames'

--
-- Define the functions liftC, definition, and defCom
--

-- Lift command to salsa, that is run the command and use new state.
liftC :: SalsaCommand a -> Salsa a
liftC sc = 
    Salsa $ \con@(Context env state) frames -> 
        let (a, state') = runSC sc con
            frames' = calcFrames state state' env ++ frames
        in (a, Context env state', frames')
    where 
        -- Takes start and end state and environment.
        -- Combines frames calculated for each shape by calcGpxInstrs.
        -- Returns the frames between the two current key frames
        calcFrames :: State -> State -> Environment -> [Frame]
        calcFrames s1 s2 env = 
            {-for each list of frames in the list, combine first elems(gpxInstr) 
              to one list of gpxInstr, that is, a frame. -}
            map concat $
            L.transpose $
            M.elems $ --[[frame]], list of list of frame pr shape
            M.mapWithKey  
                (\sName vps1 -> 
                    case M.lookup sName s2 of
                        Just vps2 -> getShapeFrames 
                            (MB.fromMaybe (error "Shape not found in environment.") 
                                $ M.lookup sName $ shapes env)
                            vps1 vps2 (framerate env)
                        Nothing -> 
                            error "Something went wrong. Cannot find shape in both key frames"
                ) s1
        -- Takes shape, list of starting pos on views,list of ending pos on views and the framerate
        -- Returns list of frames for the shape on each view, that is , one GpxInstr pr shape pr view pr frame.
        getShapeFrames :: Shape -> [(ViewName, Position)] -> 
                          [(ViewName, Position)] -> Integer -> [Frame]
        getShapeFrames s vps1 vps2 fr = L.transpose vFrames
            where vFrames = zipWith (\(v1,pos1) (_,pos2) ->     -- Returns list of frames for each view for the given shape
                                        getShapeGpxInstrs s v1 pos1 pos2 fr) --If views are not same, then this goes wrong.
                            vps1 vps2

        -- Returns the frames for the given shape on the given view.
        getShapeGpxInstrs :: Shape -> ViewName -> Position -> 
                             Position -> Integer -> [GpxInstr]
        getShapeGpxInstrs s vn pos1 pos2 fr = 
            case s of
                NewRectangle _ _ w h c -> 
                    L.reverse $ map (\(x', y') -> DrawRect x' y' w h vn c) posList
                NewCircle _ _ r c -> 
                    L.reverse $ map (\(x', y') -> DrawCirc x' y' r vn c) posList
            where posList = interpolate fr pos1 pos2


-- Compute definition and update Salsa monad (that is, the context)
definition :: Definition -> Salsa ()
definition (Viewdef i e1 e2) = Salsa $ 
    \con@(Context env state) frame -> 
        ((),
         let e1' = checkDim $ calcExpr e1 con
             e2' = checkDim $ calcExpr e2 con
             views' = M.insert i (e1', e2') $ views env
         in Context (Environment views' (groups env) (shapes env) [i]
                  (framerate env))
            state,
        frame)
definition (Rectangle i e1 e2 e3 e4 c) = Salsa $ 
    \con@(Context env state) frame -> 
        let e1'' = calcExpr e1 con --Hlint is silly...
            e2' = calcExpr e2 con
            e3' = checkDim $ calcExpr e3 con
            e4' = checkDim $ calcExpr e4 con
            c' = getColor c
            shapes' = M.insert i (NewRectangle e1'' e2' e3' e4' c') $ shapes env
            v = map (\ident -> (ident,(e1'', e2'))) $ aViews env --Insert on active views
            state' = M.insert i v state
            frame' = case frame of 
                    [] -> [map (\vn -> DrawRect e1'' e2' e3' e4' vn c') 
                               (aViews env)]
                    f:fs -> (f ++ map (\vn -> DrawRect e1'' e2' e3' e4' vn c') 
                                      (aViews env)) : fs
        in
        ((),
         Context (Environment (views env) (groups env) 
                  shapes' (aViews env) (framerate env)) state',
         frame')
definition (Circle i e1 e2 e3 c) = Salsa $ 
    \con@(Context env state) frame -> 
        let e1' = calcExpr e1 con
            e2' = calcExpr e2 con
            e3' = checkDim $ calcExpr e3 con
            c' = getColor c
            shapes' = M.insert i (NewCircle e1' e2' e3' c') $ shapes env
            v = map (\ident -> (ident,(e1', e2'))) $ aViews env --Insert on active views
            state' = M.insert i v state
            frame' = case frame of 
                    [] -> [map (\vn -> DrawCirc e1' e2' e3' vn c') 
                               (aViews env)]
                    f:fs -> (f ++ map (\vn -> DrawCirc e1' e2' e3' vn c') 
                                      (aViews env)) : fs
        in
        ((),
         Context (Environment (views env) (groups env) 
                  shapes' (aViews env) (framerate env)) state',
         frame')
definition (View i) = Salsa $ 
    \(Context env state) frame -> 
        ((),
         Context (Environment (views env) (groups env) (shapes env) 
                  (getViewId env i) 
                  (framerate env))
            state,
         frame)
definition (Group i ids) = Salsa $ 
    \(Context env state) frame -> 
        ((),
         let groups' = M.insert i ids $ groups env
         in Context (Environment (views env) groups'
                  (shapes env) (aViews env) (framerate env))
            state,
         frame)

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com

--
-- Define the function runProg
--

runProg :: Integer -> Program -> Animation
runProg f prog = if f > 0 then (viewList, L.reverse frames) else error "Frame rate cannot be zero or negative"
    where (_, Context env _, frames) = 
                    runS (mapM defCom prog) (emptyContext f) emptyFrames
          viewList = map (\(k, (a, b)) -> (k, a, b)) $ M.toList $ views env