module Curves where

import Text.Printf

data Point = Point(Double, Double)
    deriving (Show)
type Curve = [Point]
data Axis = Vertical | Horizontal

instance Eq Point where
    Point(x1,y1) == Point(x2,y2) = abs x1-x2 < 0.01 && abs y1-y2 < 0.01

point :: (Double, Double) -> Point
point (x,y) = Point(x,y)

curve :: Point -> [Point] -> Curve
curve x xs = x:xs

connect :: Curve -> Curve -> Curve
connect x y = x++y

rotate :: Curve -> Double -> Curve
rotate c d = 
    map (\ (Point(x,y)) -> Point(negate $ cr * x - sr * y,
              negate $ sr * x + cr * y)) c
    where
        r = d*pi/180
        sr = sin r
        cr = cos r

translate :: Curve -> Point -> Curve
translate [] _ = []
translate c@(Point(x1,y1):_) (Point(x2,y2)) = 
    map (\ (Point(x,y)) -> Point(x+moveX,y+moveY)) c
    where
        moveX = x2-x1
        moveY = y2-y1

reflect :: Curve -> Axis -> Double -> Curve
reflect c a d = map (\ (Point(x,y)) -> case a of
        Vertical -> Point(d-(x-d), y)
        Horizontal -> Point(x, d-(y-d))) c

bbox :: Curve -> (Point, Point)
bbox [] = (Point(0,0), Point(0,0))
bbox c@(p:_) = foldl (\(Point(b1,b2),Point(t1,t2)) (Point(x,y)) -> 
                (Point(min b1 x, min b2 y),Point(max t1 x, max t2 y)) 
    ) (p,p) c

width :: Curve -> Double 
width c = max x1 x2 - min x1 x2
    where (Point(x1,_), Point(x2,_)) = bbox c

height :: Curve -> Double 
height c = max y1 y2 - min y1 y2
    where (Point(_,y1), Point(_,y2)) = bbox c

toList :: Curve -> [Point]
toList c = c

toSVG :: Curve -> String
toSVG c = printf "<svg xmlns=\"http://www.w3.org/2000/svg\"\
    \ width=\"%.0fpx\" height=\"%.0fpx\" version=\"1.1\">\n\
    \<g>\n%s</g>\n</svg>"
    (width c) (height c) (writeLine c "")
    where 
        writeLine :: Curve -> String -> String
        writeLine [] s = s
        writeLine (_:[]) s = s
        writeLine (Point(x1,y1):p2@(Point(x2,y2)):ps) s = 
            writeLine (p2:ps) (s++printf "<line style=\"stroke-width: 2px;\
                    \ stroke: black; fill:white\"\
                    \ x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\"/>\n" 
                    x1 x2 y1 y2)


toFile :: Curve -> FilePath -> IO ()
toFile c fp = writeFile fp (toSVG c)