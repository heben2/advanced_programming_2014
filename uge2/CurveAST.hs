module CurveAST where

type Program = [Def]
data Def = Def Ident Curve [Def] deriving (Eq, Show)
data Curve = Connect Curve Curve
--corresponds to the ++ operator
           | Over Curve Curve
--corresponds to the ^ operator
           | Translate Curve Point
--is for the -> operator
           | Scale Curve Expr
--is for ** on curves
           | Refv Curve Expr
           | Refh Curve Expr
           | Rot Curve Expr
           | Single Point
           | Id Ident
           deriving (Eq, Show)
data Point = Point Expr Expr deriving (Eq, Show)
data Expr = Mult Expr Expr
          | Add Expr Expr
          | Width Curve
          | Height Curve
          | Const Number
          deriving (Eq, Show)
{-Where Ident is a a non-empty sequence of letters, digits and underscores (_), 
that is not one of the reserved words: where, refv, refh, rot, width, or height.-}
type Ident = String
{-Number is a floating-point number satisfying the following 
regular expression: [0-9]+(\.[0-9]+)?.-}
type Number = Double


{-Tokens are separated by whitespaces (spaces, tabs, and newlines), zero 
or more for symbolic tokens, and at least one blank for alphanumeric tokens.

The curve operators ++ and ^ are left-associative and ^ binds stronger 
(has higher precedence) than ++, likewise -> binds stronger than ^, 
and so for the order of operators as listed in the grammar.

The arithmetic operators have the standard associativity and precedences.-}
